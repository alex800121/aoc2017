module Day1 where

import Paths_AOC2017
import Data.Char (digitToInt)
import Data.List (nub)
import Data.List.Split (divvy)
import Data.Tuple (swap)

day1a :: String -> Int
day1a = sum . map (digitToInt . head) . filter ((== 1) . length . nub) . divvy 2 1 . ((++) <$> id <*> (: []) . head)

day1b s = zipWith (\x y -> if x == y then digitToInt y else 0) s s'
  where
    s' = uncurry (++) $ swap $ splitAt (length s `div` 2) s

day1 :: IO (String, String)
day1 = do
  input <- init <$> (getDataDir >>= readFile . (++ "/input/input1.txt"))
  let
   finalAnsa
    = show $ day1a input
  
  let
   finalAnsb
    = show $ sum $ day1b input
  pure (finalAnsa, finalAnsb)
