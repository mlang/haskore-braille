{-# LANGUAGE FlexibleContexts #-}
module Text.Braille.Music (
  Braille(..), toChar, Sign(..), Parser,
  anyBrl, brl,
  note, rest, partialVoice, partialMeasure, voice, measure,
  parse, (<*>), (*>)
) where

import Control.Applicative ((<$>), (<*>), (*>), (<|>))
import Data.Bits (setBit, testBit, (.&.))
import Data.Functor (($>))
import Data.List (intercalate)
import Text.Parsec (lookAhead, parse, satisfy, sepBy, try, (<?>))
import Text.Parsec.Combinator (choice, many1)
import Text.Parsec.String (Parser)

-- I do not like the fact that decimal dot input is unsafe and can not
-- be validated at compile time.
--brl :: Int -> Char
--brl = toEnum . go 10240 where
--  go u 0 = u
--  -- silently ignores errors (invalid digits [0,9] and duplicates)
--  go u v = go (if isDot d then setBit u $ pred d else u) ds where
--    isDot d = d > 0 && d < 9
--    (ds,d) = v `divMod` 10

-- So just "generate" a sum type for 6-dot Braille (one-shot by-hand macro)
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
toChar = toEnum . (+ 10240) . fromEnum

brl :: Braille -> Parser Braille
brl b = satisfy (== toChar b) $> b <?> [toChar b]

anyBrl :: Parser Braille
anyBrl = (toEnum . flip (-) 0x2800 . fromEnum) <$> satisfy (unicodeBraille . fromEnum) where
  unicodeBraille c = c >= 0x2800 && c <= 0x28FF

type AugmentationDots = Int
augmentationDots = scan 0 where scan n = brl Dot3 *> scan (succ n) <|> return n

-- Braille music is inherently ambiguous.  The time signature is necessary
-- to automatically caluclate the real values of notes and rests.
data AmbiguousValue = WholeOr16th | HalfOr32th | QuarterOr64th | EighthOr128th
                    deriving (Eq, Show)

rest :: Parser Sign
rest = Rest <$> ambiguousValue <*> augmentationDots where
  ambiguousValue = choice [ brl Dot134  $> WholeOr16th
                          , brl Dot136  $> HalfOr32th
                          , brl Dot1236 $> QuarterOr64th
                          , brl Dot1346 $> EighthOr128th
                          ]

data Step = C | D | E | F | G | A | B
          deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Sign = Note AmbiguousValue Step AugmentationDots
          | Rest AmbiguousValue AugmentationDots
          -- more to be added (Chord, ...)
          deriving (Eq, Show)

note :: Parser Sign
note = try parseNote where
  parseNote = Note <$> ambiguousValue <*> step <*> augmentationDots <?> "note"
  ambiguousValue = lookAhead $ anyBrl >>= check where
    check d = case toEnum (fromEnum d .&. fromEnum Dot36) of
              Dot36  -> return WholeOr16th
              Dot3   -> return HalfOr32th
              Dot6   -> return QuarterOr64th
              NoDots -> return EighthOr128th
  step = anyBrl >>= check where
    check d = case toEnum (fromEnum d .&. fromEnum Dot1245) of
              Dot145    -> return C
              Dot15     -> return D
              Dot124    -> return E
              Dot1245   -> return F
              Dot125    -> return G
              Dot24     -> return A
              Dot245    -> return B
              otherwise -> fail "Not a note"

type PartialVoice = [Sign]

partialVoice = many1 $ note <|> rest

type PartialMeasure = [PartialVoice]

partialMeasure = sepBy partialVoice $ brl Dot5 *> brl Dot2

type Voice = [PartialMeasure]

voice = sepBy partialMeasure $ brl Dot46 *> brl Dot13

type Measure = [Voice]

measure = sepBy voice $ brl Dot126 *> brl Dot345

-- TBC: Disambiguate values by recursively generating all possibilities and
-- only accepting those which add up to the time signature.

