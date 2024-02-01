module Day5 where

import Control.Monad.ST (ST, runST)
import qualified Data.Vector as V
import Data.Vector.Mutable (STVector)
import qualified Data.Vector.Mutable as M

step :: Int -> Int -> Int -> STVector s Int -> ST s Int
step n i acc v =
  M.readMaybe v i >>= \case
    Nothing -> pure acc
    Just x ->
      M.modify v (+ if x >= 3 then n else 1) i
        >> step n (i + x) (1 + acc) v

day5 :: IO ()
day5 = do
  input <- map (read @Int) . lines <$> readFile "input/input5.txt"
  -- input <- map (read @Int) . lines <$> readFile "input/test5.txt"
  print $ runST (step 1 0 0 =<< V.thaw (V.fromList input))
  print $ runST (step (-1) 0 0 =<< V.thaw (V.fromList input))
