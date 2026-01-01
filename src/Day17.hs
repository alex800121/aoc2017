module Day17 (day17) where

import Data.Sequence qualified as Seq
import Paths_AOC2017

initSeq = (0, Seq.singleton 0)

{-
(0) len = 1, pos = 0 -> (1 - 0 - 1) divMod 3 (0, 0), (2 - 0) mod 3 + 1 = 1
0 (1) len = 2, pos = 1 -> (2 - 1 - 1) divMod 3 = (0, 0), (2 - 0) mod 3 + 1 = 1
0 (2) 1 len = 3, pos = 1 -> (3 - 1 - 1) divMod 3 = (0, 1), (2 - 1) mod 3 + 1 = 2
0  2 (3) 1 len = 4, pos = 2 -> (4 - 2 - 1) divMod 3 = (0, 1), (2 - 1) mod 3 + 1 = 2
0  2 (4) 3  1 len = 5, pos = 2 -> (5 - 2 - 1) divMod 3 = (0, 2), (2 - 2) mod 3 + 1 = 1
0 (5) 2  4  3  1 len = 6, pos = 1 -> (6 - 1 - 1) divMod 3 = (1, 1), (2 - 1) mod 3 + 1 = 2
0  5  2  4  3 (6) 1 skip
0  5 (7) 2  4  3  6  1 len = 8, pos = 2 -> (8 - 2 - 1) divMod 3 = (1, 2), (2 - 2) mod 3 + 1 = 1
0  5  7  2  4  3 (8) 6  1 skip
0 (9) 5  7  2  4  3  8  6  1 len = 10, pos = 1 -> (10 - 1 - 1) divMod 3 = (2, 2), (2 - 2) mod 3 + 1 = 1
0 9 5 7 2 (10) 4 3 8 6 1 skip
0 9 5 7 2 10 4 3 8 (11) 6 1 skip
0 (12) 9 5 7 2 10 4 3 8 11 6 1 len = 13, pos = 1 -> (13 - 1 - 1) divMod 3 = (3, 2), (2 - 2) mod 3 + 1 = 1
0 12 9 5 7 (13) 2 10 4 3 8 11 6 1 skip
0 12 9 5 7 13 2 10 4 (14) 3 8 11 6 1 skip
0 12 9 5 7 13 2 10 4 14 3 8 11 (15) 6 1 skip
0 (16) 12 9 5 7 13 2 10 4 14 3 8 11 15 6 1 len = 17, pos = 1 -> (17 - 1 - 1) divMod 3 = (5, 0), (2 - 0) mod 3 + 1 = 3
0 16 12 9 5 (17) 7 13 2 10 4 14 3 8 11 15 6 1 skip
0 16 12 9 5 17 7 13 2 (18) 10 4 14 3 8 11 15 6 1 skip
0 16 12 9 5 17 7 13 2 18 10 4 14 (19) 3 8 11 15 6 1 skip
0 16 12 9 5 17 7 13 2 18 10 4 14 19 3 8 11 (20) 15 6 1 skip
0 16 12 9 5 17 7 13 2 18 10 4 14 19 3 8 11 20 15 6 1 (21) skip
0 16 12 (22) 9 5 18 7 13 2 19 10 4 14 20 3 8 11 21 15 6 1 21 len = 23, pos = 3 -> (23 - 3 - 1) divMod 3 = (6, 1), (2 - 1) mod 3 + 1 = 2
-}

step n (pos, s) i = (pos' + 1, s')
  where
    len = Seq.length s
    pos' = (pos + n) `mod` len
    s' = Seq.insertAt pos' i s

day17b :: Int -> Int -> Int -> Int -> Int -> Int
day17b !st !limit !pos !cur !acc
  | cur' > limit = acc
  | otherwise = day17b st limit pos' cur' (if pos' == 1 then cur' else acc)
  where
    (skip, m) = (cur - pos) `divMod` st
    pos' = ((-1 - m) `mod` st) + 1
    cur' = cur + skip + 1

day17 :: IO (String, String)
day17 = do
  input <- (read @Int) <$> (getDataDir >>= readFile . (++ "/input/input17.txt"))
  let
    finalAnsa =
      show $ (\x -> let Just n = Seq.elemIndexL 2017 x in Seq.index x (n + 1)) $ snd $ foldl' (step input) initSeq [1 .. 2017]
    finalAnsb =
      show $ day17b input 50000000 0 0 0
  pure (finalAnsa, finalAnsb)
