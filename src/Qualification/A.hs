module Qualification.A where

import           Data.Maybe (listToMaybe)
import           Data.Set   (Set)
import qualified Data.Set   as S

{-

Bleatrix Trotter the sheep has devised a strategy that helps her fall
asleep faster. First, she picks a number N. Then she starts naming N,
2 × N, 3 × N, and so on. Whenever she names a number, she thinks about
all of the digits in that number. She keeps track of which digits (0,
1, 2, 3, 4, 5, 6, 7, 8, and 9) she has seen at least once so far as
part of any number she has named. Once she has seen each of the ten
digits at least once, she will fall asleep.

Bleatrix must start with N and must always name (i + 1) × N directly
after i × N. For example, suppose that Bleatrix picks N = 1692. She
would count as follows:

    N = 1692. Now she has seen the digits 1, 2, 6, and 9.
    2N = 3384. Now she has seen the digits 1, 2, 3, 4, 6, 8, and 9.
    3N = 5076. Now she has seen all ten digits, and falls asleep.

What is the last number that she will name before falling asleep? If
she will count forever, print INSOMNIA instead.

-}

data Result = AsleepAt Int | Insomnia

instance Show Result where
  show (AsleepAt n) = show n
  show Insomnia = "INSOMNIA"


digits :: Int -> Set Char
digits = S.fromList . show


solve :: Int -> Int -> Result
solve n limit = case listToMaybe $ dropWhile (\(_, ds) -> S.size ds < 10) $ take limit $ digitstream n
  of Just (i, _) -> AsleepAt i
     Nothing -> Insomnia

digitstream :: Int -> [(Int, Set Char)]
digitstream n = iterate f (n, digits n)
  where
    f (i, ds) = (i', ds `S.union` digits i')
      where i' = i + n


defaultLimit = 1000000


runFile :: FilePath -> FilePath -> IO ()
runFile inf outf = do
   _:inputs <- map read <$> lines <$> readFile inf
   writeFile outf $ unlines $ zipWith (curry output) [1..] (map (flip solve defaultLimit) inputs)
         where output (i, res) = "Case #" ++ show i ++ ": " ++ show res

