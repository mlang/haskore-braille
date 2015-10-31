{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}

-- | Braille music to Haskore conversion functions.
module Haskore.Interface.Braille (
  -- | Errors that can happen during parsing or post-processing.
  Error(..)
  -- | Given a meter, convert Braille music to a Haskore Standard Melody.
, toStdMelody
) where

import           Control.Applicative ( Alternative((<|>))
                                     , Applicative(pure, (<*>))
                                     , many, optional, some, (<$>), (*>))
import           Control.Monad (ap, guard, liftM, when)
import           Control.Monad.Error (throwError)
import           Control.Monad.Loops (untilM)
import           Control.Monad.Trans.List (ListT(..))
import           Control.Monad.Trans.State ( StateT(..)
                                           , evalStateT, get, gets, put)
import           Data.Bits (Bits((.&.)))
import           Data.Foldable ( Foldable(fold, foldl')
                               , asum, concat, concatMap, sum, toList)
import           Data.Function (on)
import           Data.Functor (($>))
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as Map (findWithDefault, fromList, insert)
import           Data.Monoid (Monoid(mempty, mappend), mconcat)
import           Data.Ord (comparing)
import           Data.Traversable (Traversable(sequenceA, traverse), mapAccumL)
import qualified Haskore.Basic.Pitch as Pitch ( Class(..), Octave, T, Relative
                                              , transpose)
import qualified Haskore.Interface.MIDI.Render as MIDIRender
import qualified Haskore.Melody as Melody (note)
import qualified Haskore.Melody.Standard as Melody (T, na)
import qualified Haskore.Music as Music (Dur, chord, line, rest)
import qualified Haskore.Music.GeneralMIDI as MIDIMusic ( Instrument(..), T
                                                        , fromStdMelody)
import           Numeric.NonNegative.Wrapper (toNumber)
import           Prelude hiding (concat, concatMap, sum)
import qualified Sound.MIDI.File.Save as SaveMIDI
import           Text.Parsec ( Parsec, SourceName, SourcePos
                             , getPosition, getState
                             , lookAhead, newline, putState, runParser
                             , satisfy, sepBy1, space, try, (<?>))
import           Text.Parsec.Combinator (choice)
import           Text.Parsec.Error (ParseError)

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

data SixDots = NoDots | Dot1 | Dot2 | Dot12 | Dot3 | Dot13 | Dot23 | Dot123
             | Dot4 | Dot14 | Dot24 | Dot124 | Dot34 | Dot134 | Dot234
             | Dot1234 | Dot5 | Dot15 | Dot25 | Dot125 | Dot35 | Dot135
             | Dot235 | Dot1235 | Dot45 | Dot145 | Dot245 | Dot1245 | Dot345
             | Dot1345 | Dot2345 | Dot12345 | Dot6 | Dot16 | Dot26 | Dot126
             | Dot36 | Dot136 | Dot236 | Dot1236 | Dot46 | Dot146 | Dot246
             | Dot1246 | Dot346 | Dot1346 | Dot2346 | Dot12346 | Dot56 | Dot156
             | Dot256 | Dot1256 | Dot356 | Dot1356 | Dot2356 | Dot12356
             | Dot456 | Dot1456 | Dot2456 | Dot12456 | Dot3456 | Dot13456
             | Dot23456 | Dot123456
             deriving (Bounded, Enum, Eq, Read, Show)

-- | Convert to Unicode Braille.
toChar :: SixDots -> Char
toChar = toEnum . (+ 0x2800) . fromEnum

-- | A Parsec based parser type that keeps track of the last seen pitch.
type Parser = Parsec String (Maybe Pitch.T)

parse :: Parser a -> Maybe Pitch.T -> SourceName -> String -> Either ParseError a
parse = runParser

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
                   , ambiguousPitch            :: Pitch.T
                   , ambiguousValue            :: AmbiguousValue
                   , ambiguousAugmentationDots :: AugmentationDots
                   , ambiguousEnd              :: SourcePos
                   , calculatedAlteration      :: Pitch.Relative
                   }
   | AmbiguousRest { ambiguousBegin            :: SourcePos
                   , ambiguousValue            :: AmbiguousValue
                   , ambiguousAugmentationDots :: AugmentationDots
                   , ambiguousEnd              :: SourcePos
                   }
          -- more to be added (Chord, ...)
          deriving (Eq, Show)

savePitch :: Pitch.T -> Parser Pitch.T
savePitch = (*>) . putState . Just <*> pure

forgetPitch :: Parser ()
forgetPitch = putState Nothing

-- | Parse a Braille music note.
noteP :: Parser AmbiguousSign
noteP = try parseNote where
  parseNote = AmbiguousNote <$> getPosition
                            <*> optional accidentalP
                            <*> pitchP
                            <*> ambiguousValueP
                            <*> augmentationDotsP
                            <*> getPosition
                            <*> pure 0
                            <?> "note"
  ambiguousValueP = anyBrl >>= getValue where
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
  pitchP = do o <- optional octaveP
              c <- lookAhead stepP
              case o of
                Just o'  -> savePitch (o', c)
                Nothing -> do p <- getState
                              case p of
                                Just p' -> savePitch $ nextPitch p' c
                                Nothing -> fail "Missing octave mark"
  mask m dots = toEnum (fromEnum dots .&. fromEnum m)

nextPitch :: Pitch.T -> Pitch.Class -> Pitch.T
nextPitch (o, Pitch.A) c@Pitch.C = (o+1, c)
nextPitch (o, Pitch.B) c@Pitch.C = (o+1, c)
nextPitch (o, Pitch.B) c@Pitch.D = (o+1, c)
nextPitch (o, Pitch.D) c@Pitch.B = (o-1, c)
nextPitch (o, Pitch.C) c@Pitch.B = (o-1, c)
nextPitch (o, Pitch.C) c@Pitch.A = (o-1, c)
nextPitch (o, _)       c         = (o,   c)

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

newtype Parallel a
      = Parallel [a]
      deriving (Foldable, Functor, Read, Show, Traversable)

instance HasDuration a => HasDuration (Parallel a) where
  dur (Parallel [])    = 0
  dur (Parallel (x:_)) = dur x

parallelSepBy1 :: Parser a -> Parser b -> Parser (Parallel a)
parallelSepBy1 p sep = do xs <- sepBy1 p $ sep *> forgetPitch
                          when (length xs > 1) forgetPitch
                          pure $ Parallel xs

newtype Sequential a
      = Sequential [a]
      deriving (Applicative, Foldable, Functor, Monoid, Read, Show, Traversable)

instance HasDuration a => HasDuration (Sequential a) where dur = sum . fmap dur

-- | A Braille music measure can contain parallel and serial music.
-- The inner most voice is called partial voice because it can potentially
-- span just across a part of the whole measure.
type AmbiguousPartialVoice = Sequential AmbiguousSign

partialVoiceP :: Parser AmbiguousPartialVoice
partialVoiceP = Sequential <$> some (noteP <|> restP)

-- | A partial measure contains parallel partial voices.
type AmbiguousPartialMeasure = Parallel AmbiguousPartialVoice

partialMeasureP :: Parser AmbiguousPartialMeasure
partialMeasureP = parallelSepBy1 partialVoiceP
                $ brl Dot5 *> brl Dot2

-- | A voice consists of one or more sequential partial measures.
type AmbiguousVoice = Sequential AmbiguousPartialMeasure

voiceP :: Parser AmbiguousVoice
voiceP = Sequential <$> sepBy1 partialMeasureP sep where
  sep = brl Dot46 *> brl Dot13

-- | A measure contains several parallel voices.
type AmbiguousMeasure = Parallel AmbiguousVoice

measureP :: Parser AmbiguousMeasure
measureP = parallelSepBy1 voiceP
         $ brl Dot126 *> brl Dot345

measureSepP :: Parser ()
measureSepP =  (space *> return ())
           <|> (brl NoDots *> return ())
           <|> (newline *> return ())

sectionP :: Parser [AmbiguousMeasure]
sectionP = sepBy1 measureP measureSepP

-- With the basic data structure defined and parsed into, we can finally
-- move towards the actually interesting task of disambiguating values.

data Sign = Note { realValue :: Music.Dur, ambiguous :: AmbiguousSign }
          | Rest { realValue :: Music.Dur, ambiguous :: AmbiguousSign }
          -- more to be added (Chord, ...)
          deriving (Eq, Show)
instance HasDuration Sign where
  dur = (*) <$> realValue
            <*> augmentationDotsFactor . ambiguousAugmentationDots . ambiguous

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

data PartialVoice = PartialVoice
                    { pvDur :: Music.Dur
                    , pvContent :: Sequential Sign
                    }
                  deriving (Show)

instance HasDuration PartialVoice where dur = pvDur

mkPV :: Sequential Sign -> PartialVoice
mkPV signs = PartialVoice (dur signs) signs

type PartialMeasure = Parallel PartialVoice
type Voice = Sequential PartialMeasure
type Measure = Parallel Voice

parallel :: HasDuration b
         => (a -> Either e [b])
         -> Parallel a
         -> Either e [Parallel b]
parallel f = fmap (filter allEqDur . sequenceA) . traverse f where
  allEqDur (Parallel [])     = False
  allEqDur (Parallel (x:xs)) = all (eq x) xs where eq = (==) `on` dur

measures :: Music.Dur
         -> AmbiguousMeasure
         -> Either SemanticError [Measure]
-- | Given the current time signature (meter), return a list of all possible
-- interpretations of the given measure.
measures = parallel . voices

voices :: Music.Dur
       -> AmbiguousVoice
       -> Either SemanticError [Voice]
voices _ (Sequential []) = throwError EmptyVoice
voices l (Sequential xs) = go l xs where
  go _ []     = pure $ pure mempty
  go l (x:xs) = do ys <- partialMeasures l x
                   fmap mconcat $ sequenceA $ do
                     y <- ys
                     pure $ do
                       yss <- go (l - dur y) xs
                       pure $ (pure y `mappend`) <$> yss

partialMeasures :: Music.Dur
                -> AmbiguousPartialMeasure
                -> Either SemanticError [PartialMeasure]
partialMeasures = parallel . partialVoices

partialVoices :: Music.Dur
              -> AmbiguousPartialVoice
              -> Either SemanticError [PartialVoice]
partialVoices = curry $ fmap (map mkPV) . runListT . evalStateT
                        (allWhich $ notegroup <|> one large <|> one small)

type Disambiguator s e = StateT s (ListT (Either e))
type PVDisambiguator = Disambiguator
                       (Music.Dur, AmbiguousPartialVoice)
                       SemanticError (Sequential Sign)

allWhich :: PVDisambiguator -> PVDisambiguator
-- In newer GHC, null . Foldable.toList = Foldable.null
allWhich = liftM fold . (`untilM` gets $ null . toList . snd)

one :: (AmbiguousValue -> Music.Dur) -> PVDisambiguator
one mk = do (l, Sequential (x:xs)) <- get
            let line = pure $ mkSign mk x
            guard (l >= dur line)
            put (l - dur line, Sequential xs)
            pure line

notegroup :: PVDisambiguator
notegroup = do Sequential (x:xs) <- gets snd
               asum $ map (tryLine $ mkSign small x) $ spans body xs where
  body (AmbiguousNote {ambiguousValue = av}) = av == EighthOr128th
  body _ = False                                  
  tryLine :: Sign -> ([AmbiguousSign], [AmbiguousSign]) -> PVDisambiguator
  tryLine a (as, xs) = do guard $ length as >= 3 -- Check minimal length of a notegroup 
                          let line = Sequential $ a : map (mkSign $ const $ realValue a) as
                          let d = dur line
                          l <- gets fst
                          guard $ l >= d         -- Does the choosen line fit?
                          put (l - d, Sequential xs)
                          pure line

-- | Like 'span' but gives all combinations till predicate fails.
spans :: (a -> Bool) -> [a] -> [([a], [a])]
spans = go mempty where
  go _ _ []     = []
  go i p (x:xs) | p x       = let i' = i `mappend` pure x in (i',xs) : go i' p xs
                | otherwise = []

flatten :: Measure -> [Sign]
flatten = concatMap $ concatMap $ concatMap $ toList . pvContent

harmonicMean :: Fractional a => [a] -> a
harmonicMean = uncurry (flip (/)) . foldl' (\(x, y) n -> (x+1/n, y+1)) (0, 0)

score :: Measure -> Rational
score = harmonicMean . map (toNumber . dur) . flatten

scoreList :: [Measure] -> [(Rational, Measure)]
scoreList = sortBy (flip (comparing fst)) . map ((,) =<< score)

pick :: [Measure] -> Either [(Rational, Measure)] Measure
pick []         = Left  mempty
pick [x]        = Right x
pick xs@(_:_:_) = let sorted@((bestScore, best) : xs') = scoreList xs
                      plausible = (> 2/3) . (/ bestScore) . fst
                  in case takeWhile plausible xs' of
                       [] -> Right best
                       _  -> Left sorted

-- | Maps diatonic pitches to their currently active accidentals.
type AccidentalMap = Map Pitch.T Pitch.Relative

type Fifths = Int

accidentals :: Fifths -> Map Pitch.T Pitch.Relative
accidentals k = Map.fromList [ ((o, c), a)
                             | o <- [0..maxOctave]
                             , (c, a) <- zip diatonicSteps $ fifths k
                             , a /= 0
                             ] where
  maxOctave = 9
  diatonicSteps = [Pitch.C, Pitch.D, Pitch.E, Pitch.F, Pitch.G, Pitch.A, Pitch.B]
  fifths n | n > 0 = rotateR 4 $ case rotateR 6 $ fifths $ n-1 of x:xs -> succ x:xs
           | n < 0 = rotateR 1 $ case rotateR 3 $ fifths $ n+1 of x:xs -> pred x:xs
           | otherwise = replicate 7 0 where
    rotateR n = zipWith const <$> drop n . cycle <*> id

type MeasureAddress = (Int, Int, Int, Int)

calculateAlterations :: AccidentalMap -> Measure -> (AccidentalMap, Measure)
calculateAlterations acc m = foldl' visit (acc, m) (timewise m) where
  timewise :: Measure -> [(Sign, MeasureAddress)]
  timewise = map snd . sortBy (comparing fst)
           . concat . zipWith3 twV ((,,,) <$> [0..]) (repeat 0) . toList where
    twV :: (Int -> Int -> Int -> MeasureAddress) -> Music.Dur -> Voice
        -> [(Music.Dur, (Sign, MeasureAddress))]
    twV a pos = concat . snd . mapAccumL accum (0, pos) where
      accum (ix, pos) x = ((ix+1, pos + dur x), twPM (a ix) pos x) where
        twPM :: (Int -> Int -> MeasureAddress) -> Music.Dur -> PartialMeasure
             -> [(Music.Dur, (Sign, MeasureAddress))]
        twPM a pos = concat . zipWith3 twPV (a <$> [0..]) (repeat pos) . toList where
          twPV :: (Int -> MeasureAddress) -> Music.Dur -> PartialVoice
               -> [(Music.Dur, (Sign, MeasureAddress))]
          twPV a pos = toList . snd . mapAccumL accum (0, pos) . pvContent where
            accum (ix, pos) x = ((ix+1, pos + dur x), (pos, (x, a ix)))

  visit :: (AccidentalMap, Measure) -> (Sign, (Int, Int, Int, Int))
        -> (AccidentalMap, Measure)
  visit (acc, m) (Note d u, ix) =
    let acc' = maybe acc (flip (Map.insert $ ambiguousPitch u) acc . alter)
             $ ambiguousAccidental u
        a = Map.findWithDefault 0 (ambiguousPitch u) acc'
    in (acc', updateMeasure ix (updateAlter a) m) where
      updateAlter a (Note d u) = Note d $ u { calculatedAlteration = a }
      updateAlter _ x = x
  visit (acc, m) _ = (acc, m)

  updateMeasure :: (Int, Int, Int, Int) -> (Sign -> Sign) -> Measure -> Measure
  updateMeasure (a, b, c, d) f = updL a (updateV (b, c, d)) where
    updateV (b, c, d) = updL b (updatePM (c, d)) where
      updatePM (c, d) = updL c (updatePV d) where
        updatePV d (PartialVoice d' l) = PartialVoice d' $ updL d f l
    updL i f = snd . mapAccumL (\i' x -> (i'+1, if i==i' then f x else x)) 0

data SemanticError = EmptyVoice
                   | NoPreviousMeasure SourcePos
                   deriving (Show)

    

data Error = Syntax ParseError      -- ^ Error while parsing input.
           | Semantic SemanticError -- ^ Error in the post-processing phase.
           deriving (Show)

-- | Test measure disambiguation.
testms :: Music.Dur -> String -> Either Error [Measure]
testms l = either e1 (either e2 Right . measures l) . parse parser Nothing "test input" where
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
       do candidates <- testms l "⠐⠺⠓⠳⠛⠭⠭⠚⠪⠑⠣⠜⠭⠐⠵⠽⠨⠅⠾⠮⠚⠽⠾⠮⠾⠓⠋⠑⠙⠛⠊"
          pure $ filter ((== l) . dur) candidates


measureToHaskore :: Measure -> Melody.T
measureToHaskore = Music.chord . toList . fmap voice where
  voice :: Voice -> Melody.T
  voice = Music.line . toList . fmap partialMeasure where
    partialMeasure :: PartialMeasure -> Melody.T
    partialMeasure = Music.chord . toList . fmap partialVoice where
      partialVoice :: PartialVoice -> Melody.T
      partialVoice = Music.line . toList . fmap conv . pvContent where
        conv :: Sign -> Melody.T
        conv r@(Rest {}) = Music.rest $ dur r
        conv n@(Note {ambiguous = u}) = Melody.note (pitch u) (dur n) Melody.na where
          pitch :: AmbiguousSign -> Pitch.T
          pitch = Pitch.transpose <$> calculatedAlteration <*> ambiguousPitch

toStdMelody :: Music.Dur -> String -> Either Error Melody.T
toStdMelody l b = do ms <- testms l b
                     either (const $ Left $ Semantic EmptyVoice)
                            (Right . measureToHaskore . snd . calculateAlterations (accidentals 0))
                            (pick ms)

song :: Melody.T -> MIDIMusic.T
song = MIDIMusic.fromStdMelody MIDIMusic.AcousticGrandPiano

write :: FilePath -> MIDIMusic.T -> IO ()
write file = SaveMIDI.toFile file . MIDIRender.generalMidiDeflt

gentest = either (const undefined) (write "test.mid") $
          fmap (song . Music.line . fmap measureToHaskore) test

class HasDuration a where
  dur :: a -> Music.Dur


