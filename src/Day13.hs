module Day13 where

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (isNothing, mapMaybe)
import Paths_AOC2017

{-

0: 3
1: 2
4: 4
6: 4

-}

day13b (m0, ns0) (x, y) = (m1, ns1)
  where
    d = (y - 1) * 2
    m1 = lcm m0 d
    ns1 =
      [ j
      | i <- ns0
      , j <- [i, (i + m0) .. m1]
      , (j + x) `mod` d /= 0
      ]

day13a n (x, y) = if (x + n) `mod` ((y - 1) * 2) == 0 then Just (x * y) else Nothing

day13 :: IO (String, String)
day13 = do
  input <- map ((\[x, y] -> (read @Int x, read @Int y)) . splitOn ": ") . lines <$> (getDataDir >>= readFile . (++ "/input/input13.txt"))
  let
    finalAnsa =
      show $ sum $ mapMaybe (day13a 0) input

  let
    finalAnsb = show . minimum . snd $ foldl' day13b (1, [1]) input
  pure (finalAnsa, finalAnsb)
