module C where

import           Control.Monad (replicateM)
import           Data.List     (find)
import           Data.Maybe    (catMaybes, fromJust)

toIntInBase :: Int -> String -> Int
toIntInBase base ds = sum $ zipWith f [0..] (reverse ds)
  where
    f :: Int -> Char -> Int
    f n '1' = base ^ n
    f _ '0' = 0
    f _ c = error $ "bad char: " ++ show c


-- naïve primality test
prime :: (Integral a) => a -> Bool
prime k | k <=1 = False
        | otherwise = 0 `notElem` (map (mod k)[2..k-1])

data Jamcoin = Jamcoin String [Int]
               deriving Show

toJamcoin :: String -> Maybe Jamcoin
toJamcoin digits@('1':ds) | last ds == '1'
  = if all (not . prime) interps
    then Just (Jamcoin digits (fromJust . nonTrivialDivisor <$> interps))
    else Nothing
  where
    interps = interpretations digits
toJamcoin _ = Nothing

interpretations :: String -> [Int]
interpretations digits = map (`toIntInBase` digits) [2..10]

nonTrivialDivisor :: Int -> Maybe Int
nonTrivialDivisor n = find (\i -> (n `mod` i) == 0) [2..n-2]


possibleJamcoins :: Int -> [String]
possibleJamcoins n = do
  middle <- replicateM (n - 2) "01"
  return $ "1" ++ middle ++ "1"

findJamcoins :: Int -> Int -> [Jamcoin]
findJamcoins j n = take j $ catMaybes $ toJamcoin <$> possibleJamcoins n


solve :: String -> [Jamcoin]
solve s = let [n, j] = read <$> words s in
  findJamcoins j n

runFile :: FilePath -> FilePath -> IO ()
runFile inf outf = do
   _:inputs <- lines <$> readFile inf
   writeFile outf $ unlines $ zipWith (curry output) [1..] (solve <$> inputs)
     where output (i, res) = "Case #" ++ show i ++ ":\n" ++ unlines (showcoin <$> res)
           showcoin (Jamcoin c ds) = c ++ " " ++ unwords (show <$> ds)

