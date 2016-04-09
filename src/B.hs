module B where

import           Data.List (break)

data Orientation = Happy
                 | Blank
                 deriving Eq

oflip :: Orientation -> Orientation
oflip Happy = Blank
oflip Blank = Happy

readOrientation :: Char -> Orientation
readOrientation '+' = Happy
readOrientation '-' = Blank
readOrientation c = error $ "Unknown orientation: " ++ show c

instance Show Orientation where
  show Happy = "+"
  show Blank = "-"

type Stack = [Orientation]

allhappy :: Stack -> Bool
allhappy = all (== Happy)

flips :: Stack -> [Stack]
flips stack = iterate stackflip stack

stackflip :: Stack -> Stack
stackflip [] = []
stackflip s@(o:os) = map oflip same ++ rest
  where (same, rest) = break (/= o) s

readStack :: String -> Stack
readStack = map readOrientation

solve :: String -> Int
solve input = length $ takeWhile (not . allhappy) (flips (readStack input))

runFile :: FilePath -> FilePath -> IO ()
runFile inf outf = do
   _:inputs <- lines <$> readFile inf
   writeFile outf $ unlines $ zipWith (curry output) [1..] (solve <$> inputs)
     where output (i, res) = "Case #" ++ show i ++ ": " ++ show res

