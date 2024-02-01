module Day2 where
import Data.List (find)
import Data.Maybe (mapMaybe)

-- day2b :: [Int] -> Maybe Int
day2b [] = Nothing
day2b (x : xs)
  | Just n <- find ((== 0) . snd) $ map (`divMod` x) xs = Just n
  | Just n <- find ((== 0) . snd) $ map (x `divMod`) xs = Just n
  | otherwise = day2b xs
day2 :: IO ()
day2 = do
  input <- map (map (read @Int) . words) . lines <$> readFile "input/input2.txt"
  print $ sum $ map ((-) <$> maximum <*> minimum) input
  print $ sum $ mapMaybe (fmap fst . day2b) input
