module Qualification.D where

import           Data.List (nub)


data Tile = G | L
            deriving (Eq, Show)


generations :: [Tile] -> [[Tile]]
generations tiles = iterate transform tiles
  where
    k = length tiles
    transform = concatMap replacement
    replacement L = tiles
    replacement G = replicate k G

peekPositions k c = nub $ map (\initialOffset -> ((initialOffset - 1) * c * k) + initialOffset * c) [1..k]



data Result = Possible [Int] | Impossible
instance Show Result where
  show (Possible nums) = unwords $ show <$> nums
  show Impossible = "IMPOSSIBLE"

solveSmall :: Int -> Int -> Int -> Result
solveSmall k c s | k == s = Possible [1..k]
solveSmall _ _ _ = error "you lied"

solve :: String -> Result
solve input = let [k, c, s] = read <$> words input in
  solveSmall k c s

runFile :: FilePath -> FilePath -> IO ()
runFile inf outf = do
   _:inputs <- lines <$> readFile inf
   writeFile outf $ unlines $ zipWith (curry output) [1..] (solve <$> inputs)
     where output (i, res) = "Case #" ++ show i ++ ": " ++ show res


