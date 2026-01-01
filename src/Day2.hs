module Day2 where
import Paths_AOC2017
import Paths_AOC2017
import Data.List (find)
import Data.Maybe (mapMaybe)

-- day2b :: [Int] -> Maybe Int
day2b [] = Nothing
day2b (x : xs)
  | Just n <- find ((== 0) . snd) $ map (`divMod` x) xs = Just n
  | Just n <- find ((== 0) . snd) $ map (x `divMod`) xs = Just n
  | otherwise = day2b xs
day2 :: IO (String, String)
day2 = do
  input <- map (map (read @Int) . words) . lines <$> (getDataDir >>= readFile . (++ "/input/input2.txt"))
  let
   finalAnsa
    = show $ sum $ map ((-) <$> maximum <*> minimum) input
  
  let
   finalAnsb
    = show $ sum $ mapMaybe (fmap fst . day2b) input
  pure (finalAnsa, finalAnsb)
