module Day4 where
import Data.List (nub, sort)

day4 :: IO ()
day4 = do
  input <- map words . lines <$> readFile "input/input4.txt"
  print $ length $ filter ((==) <$> length <*> length . nub) input
  print $ length $ filter ((==) <$> length <*> length . nub) $ map (map sort) input
