module C where

import           Control.Monad       (replicateM)
--import           Data.Char           (intToDigit)
import           Data.List           (find)
import           Data.Maybe          (catMaybes, fromJust)
import           Data.Numbers.Primes (isPrime)
--import           Numeric             (showIntAtBase)

toIntInBase :: Integer -> String -> Integer
toIntInBase base ds = sum $ zipWith f [0..] (reverse ds)
  where
    f :: Integer -> Char -> Integer
    f n '1' = base ^ n
    f _ '0' = 0
    f _ c = error $ "bad char: " ++ show c


data Jamcoin = Jamcoin String [Integer]
               deriving Show

toJamcoin :: String -> Maybe Jamcoin
toJamcoin digits@('1':ds) | last ds == '1'
  = if all (not . isPrime) interps
    then Just (Jamcoin digits (nonTrivialDivisor <$> interps))
    else Nothing
  where
    interps = interpretations digits
toJamcoin _ = Nothing

interpretations :: String -> [Integer]
interpretations digits = map (`toIntInBase` digits) [2..10]

nonTrivialDivisor :: Integer -> Integer
nonTrivialDivisor n = fromJust $ find (\i -> (n `mod` i) == 0) [2..n-2]

possibleJamcoins :: Integer -> [String]
possibleJamcoins n = do
  middle <- replicateM (fromIntegral (n - 2)) "01"
  return $ "1" ++ middle ++ "1"
--possibleJamcoins n = map toBinString [min, (min+2)..max]
--  where
--    min = 1 + 2 ^(n-1)
--    max = 2^n - 1
--    toBinString ds = (showIntAtBase 2 intToDigit ds) ""
--

findJamcoins :: Integer -> Integer -> [Jamcoin]
findJamcoins j n = take (fromIntegral j) $ catMaybes $ toJamcoin <$> possibleJamcoins n


solve :: String -> [Jamcoin]
solve s = let [n, j] = read <$> words s in
  findJamcoins j n

runFile :: FilePath -> FilePath -> IO ()
runFile inf outf = do
   _:inputs <- lines <$> readFile inf
   writeFile outf $ unlines $ zipWith (curry output) [1..] (solve <$> inputs)
     where output (i, res) = "Case #" ++ show i ++ ":\n" ++ unlines (showcoin <$> res)
           showcoin (Jamcoin c ds) = c ++ " " ++ unwords (show <$> ds)

