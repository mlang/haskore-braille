module Haskore.Interface.Braille.Utilities (makeDotsType) where

import Data.Bits (testBit)
import Language.Haskell.TH (Q, Dec(DataD), Con(NormalC), mkName)

ctorNames :: [String]
ctorNames = "NoDots" : map ctorName [1..63] where
  ctorName :: Int -> String
  ctorName = (++) "Dot" . concatMap (show . succ) . flip filter [0..5] . testBit

makeDotsType :: Q [Dec]
makeDotsType = do
  let ctors = map (\n -> NormalC (mkName n) []) ctorNames
  let instances = map mkName ["Bounded", "Enum", "Eq", "Read", "Show"]
  return [DataD [] (mkName "SixDots") [] ctors instances]
