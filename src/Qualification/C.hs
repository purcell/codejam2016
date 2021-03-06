module Qualification.C where

import           Control.Monad       (guard, replicateM)
import           Data.List           (find)
import           Data.Maybe          (catMaybes, isJust)
import           Data.Numbers.Primes (primes)
import           Debug.Trace         (trace)

toIntInBase :: Integer -> String -> Integer
toIntInBase base ds = sum $ zipWith f [0..] (reverse ds)
  where
    f :: Integer -> Char -> Integer
    f n '1' = base ^ n
    f _ '0' = 0
    f _ c = error $ "bad char: " ++ show c

data Jamcoin = Jamcoin String [Integer]
               deriving Show

interpretations :: String -> [Integer]
interpretations digits = map (`toIntInBase` digits) [2..10]

somePrimes :: [Integer]
somePrimes = take 1000 primes

possibleJamcoins :: Int -> [Jamcoin]
possibleJamcoins n = do
  middle <- replicateM (n - 2) "01"
  let
    digits = "1" ++ middle ++ "1"
    primeFactor i = find (\j -> i `mod` j == 0) $ takeWhile (\j -> j * j < i) somePrimes
    primeFactors = primeFactor <$> interpretations digits
    in do
      guard $ all isJust primeFactors
      return $ Jamcoin digits (catMaybes primeFactors)

solve :: String -> [Jamcoin]
solve s = let [n, j] = read <$> words s in
  take j $ possibleJamcoins n


runFile :: FilePath -> FilePath -> IO ()
runFile inf outf = do
   _:inputs <- lines <$> readFile inf
   writeFile outf $ unlines $ zipWith output ([1..] :: [Int]) (solve <$> inputs)
     where output i res = "Case #" ++ show i ++ ":\n" ++ unlines (showcoin <$> res)
           showcoin co@(Jamcoin c ds) = trace (show co) $ c ++ " " ++ unwords (show <$> ds)

