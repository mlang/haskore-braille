{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Data.Braille.Music (
  Sign(..), AmbiguousValue(..), Step(..), AugmentationDots,
  dur,
  Measure, testms, test
) where

import Control.Applicative (pure, liftA2, (<$>), (<*>), (*>), (<|>))
import Control.Monad (guard)
import Control.Monad.Trans.State (get, put, runStateT)
import Data.Bits (setBit, testBit, (.&.))
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Traversable (traverse)
import Text.Parsec (lookAhead, parse, satisfy, sepBy, try, (<?>))
import Text.Parsec.Combinator (choice, many1)
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
augmentationDotsP = scan 0 where scan n = brl Dot3 *> scan (succ n) <|> pure n

-- Braille music is inherently ambiguous.  The time signature is necessary
-- to automatically calculate the real values of notes and rests.
data AmbiguousValue = WholeOr16th | HalfOr32th | QuarterOr64th | EighthOr128th
                    deriving (Enum, Eq, Show)

-- | The pitch class.
data Step = C | D | E | F | G | A | B
          deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A Braille music symbol.
data Sign = Note { ambiguousValue :: AmbiguousValue
                 , realValue :: Maybe Rational
                 , step :: Step
                 , augmentationDots :: AugmentationDots
                 }
          | Rest { ambiguousValue :: AmbiguousValue
                 , realValue :: Maybe Rational
                 , augmentationDots :: AugmentationDots
                 }
          -- more to be added (Chord, ...)
          deriving (Eq, Show)

-- | Parse a Braille music note.
note :: Parser Sign
note = try parseNote where
  parseNote = Note <$> ambiguousValueP
                   <*> pure Nothing
                   <*> stepP
                   <*> augmentationDotsP
                   <?> "note"
  ambiguousValueP = lookAhead $ anyBrl >>= getValue where
    getValue d = return $ case toEnum (fromEnum d .&. fromEnum Dot36) of
                          Dot36  -> WholeOr16th
                          Dot3   -> HalfOr32th
                          Dot6   -> QuarterOr64th
                          NoDots -> EighthOr128th
  stepP = anyBrl >>= check where
    check d = case toEnum (fromEnum d .&. fromEnum Dot1245) of
              Dot145    -> return C
              Dot15     -> return D
              Dot124    -> return E
              Dot1245   -> return F
              Dot125    -> return G
              Dot24     -> return A
              Dot245    -> return B
              otherwise -> fail "Not a note"

-- | Parse a Braille music rest.
rest :: Parser Sign
rest = Rest <$> ambiguousValueP <*> pure Nothing <*> augmentationDotsP where
  ambiguousValueP = choice [ brl Dot134  $> WholeOr16th
                           , brl Dot136  $> HalfOr32th
                           , brl Dot1236 $> QuarterOr64th
                           , brl Dot1346 $> EighthOr128th
                           ]

-- A Braille music measure can contain parallel and sequential music.
-- The inner most voice is called partial voice because it can potentially
-- span just across a part of the whole measure.
type PartialVoice = [Sign]

partialVoiceP = many1 $ note <|> rest

-- | A partial measure contains parallel partial voices.
type PartialMeasure = [PartialVoice]

partialMeasureP = sepBy partialVoiceP $ brl Dot5 *> brl Dot2

-- | A voice consists of one or more partial measures.
type Voice = [PartialMeasure]

voiceP = sepBy partialMeasureP $ brl Dot46 *> brl Dot13

-- | A measure contains several parallel voices.
type Measure = [Voice]

measureP = sepBy voiceP $ brl Dot126 *> brl Dot345

-- With the basic data structure defined and parsed into, we can finally
-- move towards the actually interesting task of disambiguating values.

-- | Given a maximum time, return all possible interpretations
-- of the given PartialVoice.
pvs :: Rational -> PartialVoice -> [PartialVoice]
pvs = curry $ map fst . runStateT (allWhich (large <|> small)) where
  allWhich p = do a <- p
                  (l,xs) <- get
                  guard (l >= 0)
                  case xs of []        -> return a
                             otherwise -> (a ++) <$> allWhich p
  large  = one 0
  small  = one 4
  -- ... Move rules to come which will eventually return lists with length > 1.
  one o  = do (l, x:xs) <- get
              let v = 2 ^^ (-(o + fromEnum (ambiguousValue x)))
              put (l-v, xs)
              return [x { realValue = Just v }]

pms :: Rational -> PartialMeasure -> [PartialMeasure]
pms l = filter allEqDur . traverse (pvs l)

vs :: Rational -> Voice -> [Voice]
vs l []     = return []
vs l (x:xs) = do pm <- pms l x
                 maybe [] (\d -> (pm :) <$> vs (l - d) xs) (dur pm)

-- | Given the current time signature (meter), return a list of all possible
-- interpretations of the given measure.
ms :: Rational -> Measure -> [Measure]
ms l = filter allEqDur . traverse (vs l)

-- Apparently inefficient helper.
allEqDur xs = all ((== dur (head xs)) . dur) (tail xs)

-- | Test measure disambiguation.
testms l s = ms l <$> parse measureP "" s

-- | A well-known time-consuming test case from Bach's Goldberg Variation #3.
-- In general, music with meter > 1 is more time-consuming because
-- 16th notes can also be interpreted as whole notes, and whole notes fit
-- into meter > 1.
test = let l = 3/2 in
       do candidates <- testms l "⠺⠓⠳⠛⠭⠭⠚⠪⠑⠣⠜⠭⠵⠽⠾⠮⠚⠽⠾⠮⠾⠓⠋⠑⠙⠛⠊"
          return $ length $ filter (== (Just l)) $ map dur candidates

class    Duration a              where dur :: a -> Maybe Rational
instance Duration Sign           where dur = realValue
instance Duration PartialVoice   where dur = foldl (liftA2 (+)) (pure 0) . map dur
instance Duration PartialMeasure where dur pm = case pm of [] -> Nothing; otherwise -> dur (head pm)
instance Duration Voice          where dur = foldl (liftA2 (+)) (pure 0) . map dur
instance Duration Measure        where dur m = case m of [] -> Nothing; otherwise -> dur (head m)
