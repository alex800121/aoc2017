module Day19 where

import Data.Array.Unboxed (UArray)
import qualified Data.Array.Unboxed as A
import Data.Foldable (find)
import MyLib (Direction (..), drawArray, toIndex)
import Data.Char (isAlpha)

type M = UArray Index Char

type Index = (Int, Int)

adjacent (x, y) = [(x + a, y + b) | (a, b) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]]

(a, b) +^ (c, d) = (a + c, b + d)

day19a :: M -> (Index, Direction) -> String
day19a m (i, d) 
  | not (A.inRange b i) || x == ' ' = []
  | isAlpha x = x : day19a m (iF, d)
  | x == '+' && A.inRange b iL && xL /= ' ' = x : day19a m (iL, pred d)
  | x == '+' && A.inRange b iR && xR /= ' ' = x : day19a m (iR, succ d)
  | otherwise = x : day19a m (iF, d)
  where
    b = A.bounds m
    x = m A.! i
    iF = i +^ toIndex d
    iL = i +^ toIndex (pred d)
    xL = m A.! iL
    iR = i +^ toIndex (succ d)
    xR = m A.! iR

day19 :: IO ()
day19 = do
  input <- drawArray @UArray . lines <$> readFile "input/input19.txt"
  let Just start = fst <$> find ((&&) <$> (== '|') . snd <*> (== 0) . snd . fst) (A.assocs input)
  putStrLn $ filter isAlpha $ day19a input (start, South)
  print $ length $ day19a input (start, South)
