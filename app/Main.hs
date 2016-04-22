module Main where

import           Qualification.C
import           System.Environment (getArgs)

main :: IO ()
main = do
  [input, output] <- getArgs
  runFile input output
