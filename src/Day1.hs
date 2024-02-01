module Day1 where

import Data.Char (digitToInt)
import Data.List (nub)
import Data.List.Split (divvy)
import Data.Tuple (swap)

day1a :: String -> Int
day1a = sum . map (digitToInt . head) . filter ((== 1) . length . nub) . divvy 2 1 . ((++) <$> id <*> (: []) . head)

day1b s = zipWith (\x y -> if x == y then digitToInt y else 0) s s'
  where
    s' = uncurry (++) $ swap $ splitAt (length s `div` 2) s

day1 :: IO ()
day1 = do
  input <- init <$> readFile "input/input1.txt"
  print $ day1a input
  print $ sum $ day1b input
