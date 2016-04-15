module Main where

import           C
import           System.Environment (getArgs)

main :: IO ()
main = do
  [input, output] <- getArgs
  runFile input output
