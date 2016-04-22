module Round1A.B where


data Case = Case Int [[Int]]
data Solution = Solution [Int]



solve :: Case -> Solution
solve = undefined



instance Show Solution where
  show (Solution xs) = unwords $ map show xs

runFile :: FilePath -> FilePath -> IO ()
runFile inf outf = do
   _:inputs <- lines <$> readFile inf
   writeFile outf $ unlines $ zipWith (curry output) [1..] (solve <$> parseCases inputs)
     where output (i, res) = "Case #" ++ show i ++ ": " ++ show res
           parseCases :: [String] -> [Case]
           parseCases []     = []
           parseCases (x:xs) = Case n (map read . words <$> these) : parseCases those
             where n = 2 ^ read x - 1
                   (these, those) = splitAt n xs
