module Haskore.Interface.Braille.Utilities (makeDotsType) where

import Data.Bits (testBit)
import Language.Haskell.TH (Q, Dec(DataD), Con(NormalC), mkName)

ctorNames :: [String]
ctorNames = "NoDots" : map ctorName [1..255] where
  ctorName :: Int -> String
  ctorName = (++) "Dot" . concatMap (show . succ) . flip filter [0..7] . testBit

makeDotsType :: String -> Q [Dec]
makeDotsType n = do
  let ctors = map (flip NormalC [] . mkName) (take 64 ctorNames)
  let instances = map mkName ["Bounded", "Enum", "Eq", "Read", "Show"]
  return [DataD [] (mkName n) [] ctors instances]
