module Main where

import Haskore.Interface.Braille (pick, test)

main = putStrLn $ show $ fmap pick test

