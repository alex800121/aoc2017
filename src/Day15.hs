module Day15 where

import Data.Bits
import Data.List (scanl')

a = 591

b = 393

-- a = 65
--
-- b = 8921

ga = 16807

gb = 48271

d = 2147483647

m = 2 ^ 16

n = 40000000

f x = (`mod` d) . (* x)

as = map (`mod` m) $ iterate (f ga) a

bs = map (`mod` m) $ iterate (f gb) b

n' = 5000000

as' = filter ((>= 2) . countTrailingZeros @Int) as

bs' = filter ((>= 3) . countTrailingZeros @Int) bs

day15 :: IO ()
day15 = do
  -- input <- readFile "input/input15.txt"
  print $ length $ filter id $ take (n + 1) $ zipWith (==) as bs
  print $ length $ filter id $ take (n' + 1) $ zipWith (==) as' bs'
