module Day5 where

import Paths_AOC2017
import Control.Monad.ST (ST, runST)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed.Mutable as M

step :: Int -> Int -> Int -> STVector s Int -> ST s Int
step !n !i !acc v =
  M.readMaybe v i >>= \case
    Nothing -> pure acc
    Just x ->
      M.write v i (x + (if x >= 3 then n else 1))
        >> step n (i + x) (1 + acc) v

day5 :: IO (String, String)
day5 = do
  input <- map (read @Int) . lines <$> (getDataDir >>= readFile . (++ "/input/input5.txt"))
  let
   finalAnsa
    = show $ runST (step 1 0 0 =<< V.thaw (V.fromList input))
  let
   finalAnsb
    = show $ runST (step (-1) 0 0 =<< V.thaw (V.fromList input))
  pure (finalAnsa, finalAnsb)
