-- https://code.google.com/codejam/contest/4304486/dashboard#s=p0
module Round1A.A where

solve :: String -> String
solve = foldl f ""
  where
    f s c = maximum [(c:s), s ++ [c]]

runFile :: FilePath -> FilePath -> IO ()
runFile inf outf = do
   _:inputs <- lines <$> readFile inf
   writeFile outf $ unlines $ zipWith (curry output) [1..] (solve <$> inputs)
     where output (i, res) = "Case #" ++ show i ++ ": " ++ res

