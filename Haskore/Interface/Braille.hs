{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Haskore.Interface.Braille (
  testms, test
) where

import           Control.Applicative (Alternative, Applicative, many, optional, pure, some, (<$>), (<*>), (*>), (<|>))
import           Control.Monad (guard, liftM)
import           Control.Monad.Error (throwError)
import           Control.Monad.Loops (untilM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT(..))
import           Control.Monad.Trans.List (ListT(..))
import           Control.Monad.Trans.State (StateT(..), evalStateT, get, gets, put)
import           Data.Bits ((.&.))
import           Data.Foldable (asum, fold, foldMap)
import           Data.Functor (($>))
import           Data.Monoid (Monoid, mempty, mappend, mconcat)
import           Data.Traversable (traverse, sequenceA)
import qualified Haskore.Basic.Pitch as Pitch (Class(..), Octave, Relative, transpose)
import qualified Haskore.Interface.MIDI.Render as MIDIRender
import qualified Haskore.Melody as Melody (note)
import qualified Haskore.Melody.Standard as Melody (T, na)
import qualified Haskore.Music as Music (Dur, chord, line, rest)
import qualified Haskore.Music.GeneralMIDI as MIDIMusic (Instrument(..), T, fromStdMelody)
import qualified Haskore.Process.Optimization as Optimize
import qualified Sound.MIDI.File.Save as SaveMIDI
import           Text.Parsec (SourcePos, getPosition, lookAhead, parse, satisfy, sepBy, try, (<?>))
import           Text.Parsec.Combinator (choice)
import           Text.Parsec.Error (ParseError)
import           Text.Parsec.String (Parser)

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

alter :: Accidental -> Pitch.Relative
alter a = fromEnum a - 2

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
    getValue d = pure $ case mask Dot36 d of
                          Dot36  -> WholeOr16th
                          Dot3   -> HalfOr32th
                          Dot6   -> QuarterOr64th
                          NoDots -> EighthOr128th
                          _      -> error "Unreachable"
  stepP = anyBrl >>= check where
    check d = case mask Dot1245 d of
              Dot145  -> pure Pitch.C
              Dot15   -> pure Pitch.D
              Dot124  -> pure Pitch.E
              Dot1245 -> pure Pitch.F
              Dot125  -> pure Pitch.G
              Dot24   -> pure Pitch.A
              Dot245  -> pure Pitch.B
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

large, small :: AmbiguousValue -> Music.Dur
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
ms :: Music.Dur -> AmbiguousMeasure -> Either SemanticError [Measure]
ms l = fmap (filter allEqDur . sequenceA) . traverse (vs l)

allEqDur :: HasDuration a => [a] -> Bool
allEqDur xs = all ((== dur (head xs)) . dur) (tail xs)

vs :: (Monoid (alternative Voice), Applicative alternative)
   => Music.Dur -> AmbiguousVoice -> Either SemanticError (alternative Voice)
vs _ [] = throwError EmptyVoice
vs l xs = go l xs where
  go _ []     = pure $ pure mempty
  go l (x:xs) = do ys <- pms l x
                   fmap mconcat $ sequenceA $ do
                     y <- ys
                     pure $ do
                       yss <- go (l - dur y) xs
                       pure $ (mappend $ pure y) <$> yss

pms :: Music.Dur -> AmbiguousPartialMeasure -> Either e [PartialMeasure]
pms l = fmap (filter allEqDur . sequenceA) . traverse (pvs l)

pvs :: Music.Dur -> AmbiguousPartialVoice -> Either e [PartialVoice]
pvs = curry $ fmap (map mkPV) . runListT . evalStateT
              (allWhich $ notegroup <|> one large <|> one small)

type Disambiguator s e = StateT s (ListT (Either e))
type PVDisambiguator e =
    Disambiguator (Music.Dur, AmbiguousPartialVoice) e [Sign]

allWhich p = fmap fold $ p `untilM` end where end = gets $ null . snd

one mk = do (l, x:xs) <- get
            let sign = mkSign mk x
            guard (l >= dur sign)
            put (l - dur sign, xs)
            pure [sign]

notegroup :: PVDisambiguator e
notegroup = do x:xs <- gets snd
               let a = mkSign small x
               asum $ map (uncurry $ go a) $ spans isTail xs where
  isTail n@(AmbiguousNote {}) = ambiguousValue n == EighthOr128th
  isTail _ = False                                  
  go a as xs = do guard $ length as >= 3 -- Check minimal length of a notegroup
                  let line = a : map (mkSign (const (realValue a))) as
                  let d = sum $ map dur line
                  l <- gets fst
                  guard $ l >= d         -- Does the choosen line fit?
                  put (l - d, xs)
                  pure line

-- | Like 'span' but gives all combinations till predicate fails.
spans :: (a -> Bool) -> [a] -> [([a], [a])]
spans = go [] where
  go _ _ []     = []
  go i p (x:xs) = if p x then let i' = i++[x] in (i',xs) : go i' p xs else []

-- Possible fully monadic implementation:
----------------------------------------------
-- Goals
-- * Functions with a m suffix should much the behaviour of the corresponding functions
--   above: ms -> msm, vs -> vsm, pms -> pmsm, pvs -> pvsm
-- * Be able to throw in every level of the computation, without having to manually
--   care about collapsing Eithers at each individual level.
-- * Keep the structure flexible since the algorithm isn't fully baked yet.
-- * Minimize differences between related operations.
--   What we really do is (traverse . traverse . traverse . traverse),
--   it would be nice if we could hide the details about keeping state in each
--   individaul level.
-- * It is unclear if PMMonad really needs a state, depends on how pmsm is implemented.

-- Notes
-- * pvsm seems correct already, untestable though.

type MState = (Music.Dur, Maybe Measure, Bool)
type VState = Music.Dur
type PMState = AmbiguousPartialMeasure         -- might be unnecessary
type PVState = (Music.Dur, AmbiguousPartialVoice)
type MMonad e = StateT MState (ListT (Either e))
type VMonad e = StateT VState (ListT (MMonad e))
type PMMonad e = StateT PMState (ListT (VMonad e))
type PVMonad e = StateT PVState (ListT (PMMonad e))

runMMonad :: Maybe Measure  -- Previous measure
          -> Music.Dur      -- Time signature (maximum duration)
          -> AmbiguousMeasure
          -> Either SemanticError [Measure]
runMMonad p l m = runListT $ evalStateT (msm m) (l, p, False)

msm :: AmbiguousMeasure -> MMonad SemanticError Measure
-- we'd like to set the Bool flag in MState if we've seen a Measure
-- with dur == time signature, allows cutting off the search in pvsm.
msm = fmap (filter allEqDur . sequenceA) . mapM f where -- ?????
  f x = do (l, _, _) <- get
           vs <- runListT $ evalStateT (vsm x) l
           return undefined

vsm :: AmbiguousVoice -> VMonad SemanticError Voice
vsm xs = undefined

pmsm :: PMMonad SemanticError PartialMeasure
pmsm = do l <- lift $ lift $ get   -- current remaining time
          apv:apvs <- get
          pvs <- runListT $ evalStateT pvsm (l, apv)
          put apvs
          pvss <- pmsm
          return undefined

pvsm :: PVMonad SemanticError PartialVoice
pvsm = fmap mkPV $ allWhich $ one large <|> one small -- <|> notegroup !!!

testmsm = undefined


data SemanticError = EmptyVoice
                   | NoPreviousMeasure SourcePos deriving (Show)

    

data Error = Syntax ParseError
           | Semantic SemanticError
           deriving (Show)

-- | Test measure disambiguation.
testms :: Music.Dur -> String -> Either Error [Measure]
testms l = either e1 (either e2 Right . ms l) . parse parser "" where
  parser = measureP
  e1 = Left . Syntax
  e2 = Left . Semantic


-- | A well-known time-consuming test case from Bach's Goldberg Variation #3.
-- In general, music with meter > 1 is more time-consuming because
-- 16th notes can also be interpreted as whole notes, and whole notes fit
-- into meter > 1.

-- λ> fmap length test
-- Right 57

test = let l = 3/2 in
       do candidates <- testms l "⠺⠓⠳⠛⠭⠭⠚⠪⠑⠣⠜⠭⠵⠽⠨⠅⠾⠮⠚⠽⠾⠮⠾⠓⠋⠑⠙⠛⠊"
          pure $ filter ((== l) . dur) candidates


measureToHaskore :: Measure -> Melody.T
measureToHaskore = Music.chord . map v where
  v = Music.line . map pm where
    pm = Music.chord . map pv where
      pv (PartialVoice _ xs) = Music.line $ map conv xs where
        conv (Rest duration _) = Music.rest duration
        conv (Note duration u) = Melody.note pitch duration Melody.na where
          pitch = Pitch.transpose 0 (octave, pitchClass)
          octave = 3
          pitchClass = ambiguousStep u

song :: Melody.T -> MIDIMusic.T
song = MIDIMusic.fromStdMelody MIDIMusic.AcousticGrandPiano

write :: FilePath -> MIDIMusic.T -> IO ()
write file = SaveMIDI.toFile file . MIDIRender.generalMidiDeflt

gentest = either (const undefined) (write "test.mid") $
          fmap (song . Music.line . fmap measureToHaskore) test

class HasDuration a where
  dur :: a -> Music.Dur

instance HasDuration Sign where
  -- | The duration of a disambiguated sign is the product of its value,
  -- the augmentation dots factor, and (unimplemented) stretch factor from tuplets
  -- or multi meter parallel staves.
  dur = (*) <$> realValue
            <*> augmentationDotsFactor . ambiguousAugmentationDots . ambiguous

instance HasDuration PartialVoice where
  dur (PartialVoice d _) = d -- avoid recomputing duration

instance HasDuration PartialMeasure where
  dur = dur . head

instance HasDuration Voice where
  dur = sum . map dur

instance HasDuration Measure where
  dur = dur . head
