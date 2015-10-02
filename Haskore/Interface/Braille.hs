{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Haskore.Interface.Braille (
  testms, test
) where

import Control.Applicative (many, some, (<$>), (<*>), (*>), (<|>))
import Control.Monad (guard)
import Control.Monad.Trans.State (get, put, runStateT)
import Data.Bits ((.&.))
import Data.Functor (($>))
import Data.Monoid (mappend)
import Data.Traversable (traverse)
import qualified Haskore.Basic.Pitch as Pitch (Class(..), transpose)
import Text.Parsec (lookAhead, parse, satisfy, sepBy, try, (<?>))
import Text.Parsec.Combinator (choice)
import Text.Parsec.String (Parser)

-- Braille music code only uses the old 6-dot system.  We enumerate all
-- possible combinations of dots to use the type system to avoid accidentally
-- specifying invalid dot patterns in the source code.
--
-- genDataBraille :: String
-- genDataBraille =
--     "data Braille = " ++ intercalate " | " ctors ++ " deriving (Enum, Eq)" where
--   ctors = "NoDots" : map ctorName [1..63] where
--     ctorName :: Int -> String
--     ctorName = (++) "Dot" . concatMap (show . succ) . flip filter [0..5] . testBit

data Braille = NoDots | Dot1 | Dot2 | Dot12 | Dot3 | Dot13 | Dot23 | Dot123 | Dot4
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
toChar :: Braille -> Char
toChar = toEnum . (+ 0x2800) . fromEnum

-- | Match a single Braille cell.
brl :: Braille -> Parser Braille
brl b = satisfy (== toChar b) $> b <?> [toChar b]

-- | Matches any Unicode Braille character.
anyBrl :: Parser Braille
anyBrl = toBraille <$> satisfy (isInUBrlBlock . fromEnum) where
  toBraille = toEnum . flip (-) 0x2800 . fromEnum
  isInUBrlBlock c = c >= 0x2800 && c <= 0x28FF

-- With these primitives defined, we can move onto parsing Braille music input.

type AugmentationDots = Int
augmentationDotsP :: Parser AugmentationDots
augmentationDotsP = length <$> many (brl Dot3)

-- Braille music is inherently ambiguous.  The time signature is necessary
-- to automatically calculate the real values of notes and rests.
data AmbiguousValue = WholeOr16th | HalfOr32th | QuarterOr64th | EighthOr128th
                    deriving (Enum, Eq, Show)

-- | A Braille music symbol.
data AmbiguousSign =
     AmbiguousNote { ambiguousValue :: AmbiguousValue
                   , ambiguousStep :: Pitch.Class
                   , ambiguousAugmentationDots :: AugmentationDots
                   }
   | AmbiguousRest { ambiguousValue :: AmbiguousValue
                   , ambiguousAugmentationDots :: AugmentationDots
                   }
          -- more to be added (Chord, ...)
          deriving (Eq, Show)

-- | Parse a Braille music note.
note :: Parser AmbiguousSign
note = try parseNote where
  parseNote = AmbiguousNote <$> ambiguousValueP <*> stepP <*> augmentationDotsP
                   <?> "note"
  ambiguousValueP = lookAhead $ anyBrl >>= getValue where
    getValue d = return $ case toEnum (fromEnum d .&. fromEnum Dot36) of
                          Dot36  -> WholeOr16th
                          Dot3   -> HalfOr32th
                          Dot6   -> QuarterOr64th
                          NoDots -> EighthOr128th
                          _      -> error "Unreachable"
  stepP = anyBrl >>= check where
    check d = case toEnum (fromEnum d .&. fromEnum Dot1245) of
              Dot145  -> return Pitch.C
              Dot15   -> return Pitch.D
              Dot124  -> return Pitch.E
              Dot1245 -> return Pitch.F
              Dot125  -> return Pitch.G
              Dot24   -> return Pitch.A
              Dot245  -> return Pitch.B
              _       -> fail "Not a note"

-- | Parse a Braille music rest.
rest :: Parser AmbiguousSign
rest = AmbiguousRest <$> ambiguousValueP <*> augmentationDotsP where
  ambiguousValueP = choice [ brl Dot134  $> WholeOr16th
                           , brl Dot136  $> HalfOr32th
                           , brl Dot1236 $> QuarterOr64th
                           , brl Dot1346 $> EighthOr128th
                           ]

-- A Braille music measure can contain parallel and sequential music.
-- The inner most voice is called partial voice because it can potentially
-- span just across a part of the whole measure.
type AmbiguousPartialVoice = [AmbiguousSign]

partialVoiceP :: Parser AmbiguousPartialVoice
partialVoiceP = some $ note <|> rest

-- | A partial measure contains parallel partial voices.
type AmbiguousPartialMeasure = [AmbiguousPartialVoice]

partialMeasureP :: Parser AmbiguousPartialMeasure
partialMeasureP = sepBy partialVoiceP $ brl Dot5 *> brl Dot2

-- | A voice consists of one or more partial measures.
type AmbiguousVoice = [AmbiguousPartialMeasure]

voiceP :: Parser AmbiguousVoice
voiceP = sepBy partialMeasureP $ brl Dot46 *> brl Dot13

-- | A measure contains several parallel voices.
type AmbiguousMeasure = [AmbiguousVoice]

measureP :: Parser AmbiguousMeasure
measureP = sepBy voiceP $ brl Dot126 *> brl Dot345

-- With the basic data structure defined and parsed into, we can finally
-- move towards the actually interesting task of disambiguating values.

data Sign =
     Note { originalValue :: AmbiguousValue
          , realValue :: Rational
          , step :: Pitch.Class
          , augmentationDots :: AugmentationDots
          }
   | Rest { originalValue :: AmbiguousValue
          , realValue :: Rational
          , augmentationDots :: AugmentationDots
          }
          -- more to be added (Chord, ...)
          deriving (Eq, Show)

mkSign :: Rational -> AmbiguousSign -> Sign
mkSign rv (AmbiguousNote { ambiguousValue = v
                         , ambiguousStep = s
                         , ambiguousAugmentationDots = d
                         }) =
  Note { realValue = rv
       , originalValue = v
       , step = s
       , augmentationDots = d
       }
mkSign rv (AmbiguousRest { ambiguousValue = v
                         , ambiguousAugmentationDots = d
                         }) =
  Rest { realValue = rv
       , originalValue = v
       , augmentationDots = d
       }

data PartialVoice = PartialVoice Rational [Sign]
type PartialMeasure = [PartialVoice]
type Voice = [PartialMeasure]
type Measure = [Voice]

-- | Given a maximum time, return all possible interpretations
-- of the given PartialVoice.
pvs :: Rational -> AmbiguousPartialVoice -> [PartialVoice]
pvs = curry $ map (mkPV . fst) . runStateT (allWhich (large <|> small)) where
  allWhich p = do a <- p
                  (l,xs) <- get
                  guard (l >= 0)
                  if not $ null xs then (mappend a) <$> allWhich p else return a
  large  = one 0
  small  = one 4
  -- ... Move rules to come which will eventually return lists with length > 1.
  one o  = do (l, x:xs) <- get
              let v = 2 ^^ (-(o + fromEnum (ambiguousValue x)))
              let sign = mkSign v x
              put (l - dur sign, xs)
              return [sign]
  mkPV signs = PartialVoice (sum $ map dur signs) signs

pms :: Rational -> AmbiguousPartialMeasure -> [PartialMeasure]
pms l = filter allEqDur . traverse (pvs l)

vs :: Rational -> AmbiguousVoice -> [Voice]
vs _ []     = return []
vs l (x:xs) = do pm <- pms l x
                 (pm :) <$> vs (l - dur pm) xs

-- | Given the current time signature (meter), return a list of all possible
-- interpretations of the given measure.
ms :: Rational -> AmbiguousMeasure -> [Measure]
ms l = filter allEqDur . traverse (vs l)

-- Apparently inefficient helper.
allEqDur :: Duration a => [a] -> Bool
allEqDur xs = all ((== dur (head xs)) . dur) (tail xs)

-- | Test measure disambiguation.
testms l s = ms l <$> parse measureP "" s

augmentationDotsFactor dots = pred (2 ^ succ dots) / (2 ^ dots)

-- | A well-known time-consuming test case from Bach's Goldberg Variation #3.
-- In general, music with meter > 1 is more time-consuming because
-- 16th notes can also be interpreted as whole notes, and whole notes fit
-- into meter > 1.
test = let l = 3/2 in
       do candidates <- testms l "⠺⠓⠳⠛⠭⠭⠚⠪⠑⠣⠜⠭⠵⠽⠾⠮⠚⠽⠾⠮⠾⠓⠋⠑⠙⠛⠊"
          return $ length $ filter (== l) $ map dur candidates

class Duration a where
  dur :: a -> Rational

instance Duration Sign where
  dur = product . flip map factors . flip ($) where
    factors = [realValue, augmentationDotsFactor . augmentationDots]

instance Duration PartialVoice where
  dur (PartialVoice d _) = d

instance Duration PartialMeasure where
  dur = dur . head

instance Duration Voice where
  dur = sum . map dur

instance Duration Measure where
  dur = dur . head