{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Text.Braille.Music (
  Sign(..), AmbiguousValue(..), Step(..), AugmentationDots,
  Measure, testms
) where

import Control.Applicative (pure, liftA2, (<$>), (<*>), (*>), (<|>))
import Control.Monad (guard)
import Control.Monad.Trans.State (StateT(..), get, put)
import Data.Bits (setBit, testBit, (.&.))
import Data.Functor (($>))
import Data.List (intercalate)
import Data.Traversable (traverse)
import Text.Parsec (lookAhead, parse, satisfy, sepBy, try, (<?>))
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.String (Parser)

genDataBraille :: String
genDataBraille =
    "data Braille = " ++ intercalate " | " ctors ++ " deriving (Enum, Eq)" where
  ctors = "NoDots" : map ctorName [1..63] where
    ctorName :: Int -> String
    ctorName = (++) "Dot" . concatMap (show . succ) . flip filter [0..5] . testBit

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

toChar :: Braille -> Char
toChar = toEnum . (+ 0x2800) . fromEnum

brl :: Braille -> Parser Braille
brl b = satisfy (== toChar b) $> b <?> [toChar b]

anyBrl :: Parser Braille
anyBrl = toBraille <$> satisfy (isInUBrlBlock . fromEnum) where
  toBraille = toEnum . flip (-) 0x2800 . fromEnum
  isInUBrlBlock c = c >= 0x2800 && c <= 0x28FF

type AugmentationDots = Int
augmentationDotsP = scan 0 where scan n = brl Dot3 *> scan (succ n) <|> pure n

-- Braille music is inherently ambiguous.  The time signature is necessary
-- to automatically caluclate the real values of notes and rests.
data AmbiguousValue = WholeOr16th | HalfOr32th | QuarterOr64th | EighthOr128th
                    deriving (Enum, Eq, Show)

data Step = C | D | E | F | G | A | B
          deriving (Bounded, Enum, Eq, Ord, Read, Show)

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

rest :: Parser Sign
rest = Rest <$> ambiguousValueP <*> pure Nothing <*> augmentationDotsP where
  ambiguousValueP = choice [ brl Dot134  $> WholeOr16th
                           , brl Dot136  $> HalfOr32th
                           , brl Dot1236 $> QuarterOr64th
                           , brl Dot1346 $> EighthOr128th
                           ]

type PartialVoice = [Sign]

partialVoiceP = many1 $ note <|> rest

type PartialMeasure = [PartialVoice]

partialMeasureP = sepBy partialVoiceP $ brl Dot5 *> brl Dot2

type Voice = [PartialMeasure]

voiceP = sepBy partialMeasureP $ brl Dot46 *> brl Dot13

type Measure = [Voice]

measureP = sepBy voiceP $ brl Dot126 *> brl Dot345

pvs :: Rational -> PartialVoice -> [PartialVoice]
pvs = curry $ (go =<<) . runStateT choices where
  go (a, (_,[])) = return a
  go (a, s)      = runStateT choices s >>= fmap (a ++) . go
  choices        = large <|> small
  large          = one 0
  small          = one 4
  one o          = do (l, x:xs) <- get
                      let v = 2 ^^ (-(o + fromEnum (ambiguousValue x)))
                      guard ((l - v) >= 0)
                      put (l-v, xs)
                      return [x { realValue = Just v }]

pms :: Rational -> PartialMeasure -> [PartialMeasure]
pms l = filter allEqDur . traverse (pvs l)

vs :: Rational -> Voice -> [Voice]
vs l []     = return []
vs l (x:xs) = do pm <- pms l x
                 maybe [] (\d -> (:) <$> pure pm <*> (vs (l - d) xs)) (dur pm)

ms :: Rational -> Measure -> [Measure]
ms l = filter allEqDur . traverse (vs l)

allEqDur xs = all ((== dur (head xs)) . dur) (tail xs)

testms s = ms 1 <$> parse measureP "" s

class    Duration a              where dur :: a -> Maybe Rational
instance Duration Sign           where dur = realValue
instance Duration PartialVoice   where dur = foldl (liftA2 (+)) (pure 0) . map dur
instance Duration PartialMeasure where dur pm = case pm of [] -> Nothing; otherwise -> dur (head pm)
instance Duration Voice          where dur = foldl (liftA2 (+)) (pure 0) . map dur
