module Round1A.B where

import           Data.List (group, sort)

type Height = Int

data Case = Case Int [[Height]]
data Solution = Solution [Height]

frequencies :: (Ord a, Eq a) => [a] -> [(a, Int)]
frequencies = map (\g -> (head g, length g)) . group . sort

solve :: Case -> Solution
solve (Case _ lists) = Solution $ map fst $ filter (odd . snd) $ frequencies (concat lists)

instance Show Solution where
  show (Solution xs) = unwords $ map show xs

cases :: FilePath -> IO [Case]
cases inf = do
   _:inputs <- lines <$> readFile inf
   return $ parseCases inputs
     where
       parseCases :: [String] -> [Case]
       parseCases []     = []
       parseCases (x:xs) = Case n (map read . words <$> these) : parseCases those
         where n = read x
               (these, those) = splitAt (n * 2 - 1) xs

runFile :: FilePath -> FilePath -> IO ()
runFile inf outf = do
   cs <- cases inf
   writeFile outf $ unlines $ zipWith (curry output) [1..] (solve <$> cs)
     where output (i, res) = "Case #" ++ show i ++ ": " ++ show res
