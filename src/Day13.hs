module Day13 where

import Paths_AOC2017
import Data.List.Split (splitOn)
import Data.List (find)
import Data.Maybe (mapMaybe, isNothing)

day13a n (x, y) = if (x + n) `mod` ((y - 1) * 2) == 0 then Just (x * y) else Nothing

day13 :: IO ()
day13 = do
  input <- map ((\[x, y] -> (read @Int x, read @Int y)) . splitOn ": ") . lines <$> (getDataDir >>= readFile . (++ "/input/input13.txt"))
  -- input <- map ((\[x, y] -> (read @Int x, read @Int y)) . splitOn ": ") . lines <$> readFile "input/test13.txt"
  print $ sum $ mapMaybe (day13a 0) input
  print $ find (\x -> all (isNothing . day13a x) input) [0..]
