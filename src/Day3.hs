module Day3 where

import Data.Bifunctor (Bifunctor (..))
import Data.List (find, findIndex)
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import Data.Maybe (mapMaybe)

input = 347991

-- day3a :: Int -> (Int, Int)
day3a n
  | n <= fst ne = second (+ (fst ne - n)) $ snd ne
  | n <= fst nw = first (+ (fst nw - n)) $ snd nw
  | n <= fst sw = second (subtract (fst sw - n)) $ snd sw
  | n <= fst se = first (subtract (fst se - n)) $ snd se
  | otherwise = undefined
  where
    level = ceiling ((sqrt (fromIntegral n) + 1) / 2)
    prev = level - 1
    zeroN = (2 * prev - 1) ^ 2
    lastN = (2 * level - 1) ^ 2
    sideN = (lastN - zeroN) `div` 4
    se = (lastN, (prev, prev))
    sw = bimap (subtract sideN) (first (subtract sideN)) se
    nw = bimap (subtract sideN) (second (subtract sideN)) sw
    ne = bimap (subtract sideN) (first (+ sideN)) nw

adjacent (a, b) = [(a + x, b + y) | x <- [-1 .. 1], y <- [-1 .. 1], (x, y) /= (0, 0)]

day3b :: [(Int, Int)] -> [((Int, Int), Int)]
day3b xs = (head xs, 1) : go (Map.singleton (head xs) 1) (tail xs)
  where
    go acc (x : xs) = (x, as) : go (Map.insert x as acc) xs
      where
        as = sum $ mapMaybe (acc Map.!?) $ adjacent x

day3 :: IO ()
day3 = do
  -- input <- readFile "input/input3.txt"
  print $ (+) <$> abs . fst <*> abs . snd $ day3a input
  let l = day3b $ map day3a [1 ..]
  print $ snd <$> find ((> input) . snd) l
