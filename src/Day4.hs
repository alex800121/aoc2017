module Day4 where
import Paths_AOC2017
import Paths_AOC2017
import Data.List (nub, sort)

day4 :: IO ()
day4 = do
  input <- map words . lines <$> (getDataDir >>= readFile . (++ "/input/input4.txt"))
  print $ length $ filter ((==) <$> length <*> length . nub) input
  print $ length $ filter ((==) <$> length <*> length . nub) $ map (map sort) input
