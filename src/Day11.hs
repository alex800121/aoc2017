module Day11 where

import Paths_AOC2017
import Paths_AOC2017
import Data.List.Split (splitOn)
import Data.Bifunctor (Bifunctor(..))
import Data.Foldable (Foldable(..))
import Data.List (scanl')

type Index = (Int, Int)

{-

     n  ne
          
 nw  0  se 
 
 sw  s   

-}

hexMove :: String -> Index -> Index
hexMove "s" = second (subtract 1)
hexMove "n" = second (+ 1)
hexMove "ne" = bimap (+ 1) (+ 1)
hexMove "sw" = bimap (subtract 1) (subtract 1)
hexMove "nw" = first (subtract 1)
hexMove "se" = first (+ 1)

manhattanHex :: Index -> Index -> Int
manhattanHex (x, y) (a, b)
  | x' * y' < 0 = abs x' + abs y'
  | otherwise = max m n
  where
    (x', y') = (a - x, b - y)
    m = abs x'
    n = abs y'

day11 :: IO ()
day11 = do
  input <- splitOn "," . init <$> (getDataDir >>= readFile . (++ "/input/input11.txt"))
  -- input <- splitOn "," . init <$> readFile "input/test11.txt"
  print $ manhattanHex (0, 0) $ foldl' (flip hexMove) (0, 0) input
  print $ maximum $ map (manhattanHex (0, 0)) $ scanl' (flip hexMove) (0, 0) input
