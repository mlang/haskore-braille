{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Haskore.Interface.Braille (
  testms, test
) where

import Control.Applicative (many, optional, some, (<$>), (<*>), (*>), (<|>))
import Control.Monad (guard)
import Control.Monad.Trans.State (get, gets, put, runStateT)
import Data.Bits ((.&.))
import Data.Functor (($>))
import Data.Monoid (mappend)
import Data.Traversable (traverse)
import qualified Haskore.Basic.Pitch as Pitch (Class(..), Octave, Relative, transpose)
import qualified Haskore.Music as Music (Dur)
import Text.Parsec (SourcePos, getPosition, lookAhead, parse, satisfy, sepBy, try, (<?>))
import Text.Parsec.Combinator (choice)
import Text.Parsec.String (Parser)

-- Braille music code only uses the old 6-dot system.  We enumerate all
-- possible dot patterns to use the type system to avoid accidentally
-- specifying invalid dot patterns in the source code.
--
-- genDataBraille :: String
-- genDataBraille =
--     "data Braille = " ++ intercalate " | " ctors ++ " deriving (Enum, Eq)" where
--   ctors = "NoDots" : map ctorName [1..63] where
--     ctorName :: Int -> String
--     ctorName = (++) "Dot" . concatMap (show . succ) . flip filter [0..5] . testBit

data SixDots = NoDots | Dot1 | Dot2 | Dot12 | Dot3 | Dot13 | Dot23 | Dot123 | Dot4
             | Dot14 | Dot24 | Dot124 | Dot34 | Dot134 | Dot234 | Dot1234 | Dot5
             | Dot15 | Dot25 | Dot125 | Dot35 | Dot135 | Dot235 | Dot1235 | Dot45
             | Dot145 | Dot245 | Dot1245 | Dot345 | Dot1345 | Dot2345 | Dot12345
             | Dot6 | Dot16 | Dot26 | Dot126 | Dot36 | Dot136 | Dot236 | Dot1236
             | Dot46 | Dot146 | Dot246 | Dot1246 | Dot346 | Dot1346 | Dot2346
             | Dot12346 | Dot56 | Dot156 | Dot256 | Dot1256 | Dot356 | Dot1356
             | Dot2356 | Dot12356 | Dot456 | Dot1456 | Dot2456 | Dot12456 | Dot3456
             | Dot13456 | Dot23456 | Dot123456
             deriving (Bounded, Enum, Eq, Read, Show)

-- | Convert to Unicode Braille.
toChar :: SixDots -> Char
toChar = toEnum . (+ 0x2800) . fromEnum

-- | Match a single Braille cell.
brl :: SixDots -> Parser SixDots
brl b = satisfy (== toChar b) $> b <?> [toChar b]

-- | Matches any Unicode Braille character.
anyBrl :: Parser SixDots
anyBrl = toBraille <$> satisfy (isInUBrlBlock . fromEnum) where
  toBraille = toEnum . flip (-) 0x2800 . fromEnum
  isInUBrlBlock c = c >= 0x2800 && c <= 0x28FF

-- With these primitives defined, we can move onto parsing Braille music input.

-- | In modern practice the first dot increases the duration of the basic note
-- by half of its original value.  A dotted note is equivalent to writing
-- the basic note tied to a note of half the value; or with more than one dot,
-- tied to notes of progressively halved value.
type AugmentationDots = Int

augmentationDotsP :: Parser AugmentationDots
augmentationDotsP = length <$> many (brl Dot3)

{-
The length of any given note a with n dots is therefore given by the geometric series
a_n=a\left(1 + \tfrac 12 + \tfrac 14 + \cdots + \tfrac 1{2^n}\right)=a(2-\frac 1{2^n}).
-}
augmentationDotsFactor :: AugmentationDots -> Music.Dur
augmentationDotsFactor n = 2 - 1 / (2^n)

data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
                deriving (Enum, Eq, Show, Read)

accidentalP :: Parser Accidental
accidentalP = choice [ try (brl Dot126 *> brl Dot126) $> DoubleFlat
                     ,      brl Dot126                $> Flat
                     ,      brl Dot16                 $> Natural
                     ,      brl Dot146                $> Sharp
                     , try (brl Dot146 *> brl Dot146) $> DoubleSharp
                     ]

fromAccidental :: Accidental -> Pitch.Relative
fromAccidental a = fromEnum a - 2

octaveP :: Parser Pitch.Octave
octaveP = choice [ try (brl Dot4 *> brl Dot4) $> 0
                 ,      brl Dot4              $> 1
                 ,      brl Dot45             $> 2
                 ,      brl Dot456            $> 3
                 ,      brl Dot5              $> 4
                 ,      brl Dot46             $> 5
                 ,      brl Dot56             $> 6
                 ,      brl Dot6              $> 7
                 , try (brl Dot6 *> brl Dot6) $> 8
                 ]

-- | Braille music is inherently ambiguous.  The time signature is necessary
-- to automatically calculate the real values of notes and rests.
data AmbiguousValue = WholeOr16th | HalfOr32th | QuarterOr64th | EighthOr128th
                    deriving (Enum, Eq, Read, Show)

-- | A Braille music symbol.
data AmbiguousSign =
     AmbiguousNote { ambiguousBegin            :: SourcePos
                   , ambiguousAccidental       :: Maybe Accidental
                   , ambiguousOctave           :: Maybe Pitch.Octave
                   , ambiguousValue            :: AmbiguousValue
                   , ambiguousStep             :: Pitch.Class
                   , ambiguousAugmentationDots :: AugmentationDots
                   , ambiguousEnd              :: SourcePos
                   }
   | AmbiguousRest { ambiguousBegin            :: SourcePos
                   , ambiguousValue            :: AmbiguousValue
                   , ambiguousAugmentationDots :: AugmentationDots
                   , ambiguousEnd              :: SourcePos
                   }
          -- more to be added (Chord, ...)
          deriving (Eq, Show)

-- | Parse a Braille music note.
noteP :: Parser AmbiguousSign
noteP = try parseNote where
  parseNote = AmbiguousNote <$> getPosition
                            <*> optional accidentalP
                            <*> optional octaveP
                            <*> ambiguousValueP
                            <*> stepP
                            <*> augmentationDotsP
                            <*> getPosition
                            <?> "note"
  ambiguousValueP = lookAhead $ anyBrl >>= getValue where
    getValue d = return $ case mask Dot36 d of
                          Dot36  -> WholeOr16th
                          Dot3   -> HalfOr32th
                          Dot6   -> QuarterOr64th
                          NoDots -> EighthOr128th
                          _      -> error "Unreachable"
  stepP = anyBrl >>= check where
    check d = case mask Dot1245 d of
              Dot145  -> return Pitch.C
              Dot15   -> return Pitch.D
              Dot124  -> return Pitch.E
              Dot1245 -> return Pitch.F
              Dot125  -> return Pitch.G
              Dot24   -> return Pitch.A
              Dot245  -> return Pitch.B
              _       -> fail "Not a note"
  mask m dots = toEnum (fromEnum dots .&. fromEnum m)

-- | Parse a Braille music rest.
restP :: Parser AmbiguousSign
restP = AmbiguousRest <$> getPosition
                      <*> ambiguousValueP
                      <*> augmentationDotsP
                      <*> getPosition where
  ambiguousValueP = choice [ brl Dot134  $> WholeOr16th
                           , brl Dot136  $> HalfOr32th
                           , brl Dot1236 $> QuarterOr64th
                           , brl Dot1346 $> EighthOr128th
                           ]

-- | A Braille music measure can contain parallel and serial music.
-- The inner most voice is called partial voice because it can potentially
-- span just across a part of the whole measure.
type AmbiguousPartialVoice = [AmbiguousSign]

partialVoiceP :: Parser AmbiguousPartialVoice
partialVoiceP = some $ noteP <|> restP

-- | A partial measure contains parallel partial voices.
type AmbiguousPartialMeasure = [AmbiguousPartialVoice]

partialMeasureP :: Parser AmbiguousPartialMeasure
partialMeasureP = sepBy partialVoiceP $ brl Dot5 *> brl Dot2

-- | A voice consists of one or more serial partial measures.
type AmbiguousVoice = [AmbiguousPartialMeasure]

voiceP :: Parser AmbiguousVoice
voiceP = sepBy partialMeasureP $ brl Dot46 *> brl Dot13

-- | A measure contains several parallel voices.
type AmbiguousMeasure = [AmbiguousVoice]

measureP :: Parser AmbiguousMeasure
measureP = sepBy voiceP $ brl Dot126 *> brl Dot345

-- With the basic data structure defined and parsed into, we can finally
-- move towards the actually interesting task of disambiguating values.

data Sign = Note { realValue :: Music.Dur, ambiguous :: AmbiguousSign }
          | Rest { realValue :: Music.Dur, ambiguous :: AmbiguousSign }
          -- more to be added (Chord, ...)
          deriving (Eq, Show)

data ValueKind = Large | Small deriving (Eq, Read, Show)

fromAmbiguousValue :: ValueKind -> AmbiguousValue -> Music.Dur
fromAmbiguousValue Large WholeOr16th   = 1
fromAmbiguousValue Large HalfOr32th    = 1 / 2
fromAmbiguousValue Large QuarterOr64th = 1 / 4
fromAmbiguousValue Large EighthOr128th = 1 / 8
fromAmbiguousValue Small WholeOr16th   = 1 / 16
fromAmbiguousValue Small HalfOr32th    = 1 / 32
fromAmbiguousValue Small QuarterOr64th = 1 / 64
fromAmbiguousValue Small EighthOr128th = 1 / 128

large = fromAmbiguousValue Large
small = fromAmbiguousValue Small

mkSign :: (AmbiguousValue -> Music.Dur) -> AmbiguousSign -> Sign
mkSign v n@(AmbiguousNote {}) = Note (v (ambiguousValue n)) n
mkSign v r@(AmbiguousRest {}) = Rest (v (ambiguousValue r)) r

data PartialVoice = PartialVoice Music.Dur [Sign] deriving (Eq, Show)

mkPV :: [Sign] -> PartialVoice
mkPV signs = PartialVoice (sum $ map dur signs) signs

type PartialMeasure = [PartialVoice]
type Voice = [PartialMeasure]
type Measure = [Voice]

-- | Given the current time signature (meter), return a list of all possible
-- interpretations of the given measure.
ms :: Music.Dur -> AmbiguousMeasure -> [Measure]
ms l = filter allEqDur . traverse (vs l) where
  allEqDur xs  = all ((== dur (head xs)) . dur) (tail xs)
  vs _ []     = return []
  vs l (x:xs) = pms l x >>= \pm -> (pm :) <$> vs (l - dur pm) xs where
    pms l = filter allEqDur . traverse (pvs l) where
      pvs = curry $ map (mkPV . fst) . runStateT
            (allWhich (one large <|> one small)) where
        allWhich p = do a <- p
                        xs <- gets snd
                        if not $ null xs then mappend a <$> allWhich p else return a
        -- ... Move rules to come which will eventually return lists with length > 1.
        one mk = do (l, x:xs) <- get
                    let sign = mkSign mk x
                    guard (l >= dur sign)
                    put (l - dur sign, xs)
                    return [sign]

-- | Test measure disambiguation.
testms l s = ms l <$> parse measureP "" s

-- | A well-known time-consuming test case from Bach's Goldberg Variation #3.
-- In general, music with meter > 1 is more time-consuming because
-- 16th notes can also be interpreted as whole notes, and whole notes fit
-- into meter > 1.
test = let l = 3/2 in
       do candidates <- testms l "⠺⠓⠳⠛⠭⠭⠚⠪⠑⠣⠜⠭⠵⠽⠾⠮⠚⠽⠾⠮⠾⠓⠋⠑⠙⠛⠊"
          return $ length $ filter (== l) $ map dur candidates

class HasDuration a where
  dur :: a -> Music.Dur

instance HasDuration Sign where
  -- | The duration of a disambiguated sign is the product of its value,
  -- the augmentation dots factor, and (unimplemented) stretch factor from tuplets
  -- or multi meter parallel staves.
  dur = product . flip map factors . flip ($) where
    factors = [ realValue
              , augmentationDotsFactor . ambiguousAugmentationDots . ambiguous
              -- tuplet factor here
              ]

instance HasDuration PartialVoice where
  dur (PartialVoice d _) = d -- avoid recomputing duration

instance HasDuration PartialMeasure where
  dur = dur . head

instance HasDuration Voice where
  dur = sum . map dur

instance HasDuration Measure where
  dur = dur . head
