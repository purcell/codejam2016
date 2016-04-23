module Round1A.B where

import           Data.List  (delete, sort)
import           Data.Map   (Map)
import qualified Data.Map   as M
import           Data.Maybe (fromJust)

type Height = Int

data Case = Case Int [[Height]]
data Solution = Solution [Height]

data Strip = Row Int | Col Int deriving (Eq, Show)

data Grid = Grid { gridSize        :: Int
                 , gridEmptyStrips :: [Strip]
                 , gridContents    :: Map (Int, Int) Height }
            deriving Show

gridValues :: Grid -> Strip -> [Maybe Height]
gridValues g s = map (flip M.lookup (gridContents g)) $ stripCoords g s

stripCoords :: Grid -> Strip -> [(Int, Int)]
stripCoords g s = case s of
                    Row x -> zip (repeat x) [1..n]
                    Col y -> zip [1..n] (repeat y)
  where n = gridSize g

emptyGrid :: Int -> Grid
emptyGrid n = Grid n [c x | c <- [Row, Col], x <- [1..n]] M.empty

place :: Grid -> Strip -> [Height] -> Maybe Grid
place g s hs = if all emptyOrMatch updates then
                 Just (g { gridEmptyStrips = delete s $ gridEmptyStrips g
                         , gridContents = gridContents g `M.union` M.fromList updates })
               else Nothing
  where
    emptyOrMatch (pos, h) = case M.lookup pos (gridContents g) of
                              Just h' | h' == h -> True
                              Nothing           -> True
                              _                 -> False
    updates = zip (stripCoords g s) hs

placeAll :: Grid -> [[Height]] -> [Grid]
placeAll g [] = return g
placeAll g (l:ls) = do
  strip <- nextEmptyStrips g
  case place g strip l of
    Just g' -> placeAll g' ls
    _ -> []

nextEmptyStrips :: Grid -> [Strip]
nextEmptyStrips g = take 2 (filter isRow strips) ++ take 2 (filter (not . isRow) strips)
  where
    isRow (Row _) = True
    isRow (Col _) = False
    strips = gridEmptyStrips g

arrangements :: Case -> [Grid]
arrangements (Case n ls) = placeAll (emptyGrid n) $ sort ls

solve :: Case -> Solution
solve c = Solution $ map fromJust $ gridValues grid strip
  where grid = head $ arrangements c
        strip = head $ gridEmptyStrips grid

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
