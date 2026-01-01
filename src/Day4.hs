module Day4 where
import Paths_AOC2017
import Paths_AOC2017
import Data.List (nub, sort)

day4 :: IO (String, String)
day4 = do
  input <- map words . lines <$> (getDataDir >>= readFile . (++ "/input/input4.txt"))
  let
   finalAnsa
    = show $ length $ filter ((==) <$> length <*> length . nub) input
  
  let
   finalAnsb
    = show $ length $ filter ((==) <$> length <*> length . nub) $ map (map sort) input
  pure (finalAnsa, finalAnsb)
