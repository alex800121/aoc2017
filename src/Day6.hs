{-# LANGUAGE MultiWayIf #-}

module Day6 where

import Control.Monad.ST.Strict (ST, runST)
import Data.Bits (Bits (..))
import Data.List (group, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.WideWord (Word256)
import Debug.Trace (traceShow)
import MyLib (firstCycle')
import Paths_AOC2017

step :: Vector Int -> Vector Int
step v = v''
  where
    l = V.length v
    i = V.maxIndex v
    m = v V.! i
    v' = v V.// [(i, 0)]
    l' =
      [ (x, y')
      | (x, y) <- ((,) <$> head <*> length) <$> group (sort $ map (`mod` l) [i + 1 .. i + m])
      , let y' = y + v' V.! x
      ]
    v'' = v' V.// l'

stepST :: STVector s Int -> ST s ()
stepST v = do
  (i, x) <- MV.ifoldl' (\(ia, xa) i x -> if x > xa || x == xa && i < ia then (i, x) else (ia, xa)) (0, 0) v
  MV.write v i 0
  mapM_ (MV.modify v (+ 1) . (`mod` len)) [i + 1 .. i + x]
  where
    len = MV.length v

run !n acc v = do
  (i, _) <- MV.foldl' (\(acc, n) x -> (acc `setBit` (n + x), n + x + 1)) (0 :: Word256, 0) v
  if
    | Just m <- acc Map.!? i -> pure (n, n - m)
    | otherwise -> stepST v >> run (n + 1) (Map.insert i n acc) v

searchCycle !n !acc !v
  | Just !m <- acc Map.!? i = (n, n - m)
  | otherwise = searchCycle (n + 1) (Map.insert i n acc) v'
  where
    !i = fst $ V.foldl' (\(acc, n) x -> (acc `setBit` (n + x), n + x + 1)) (0 :: Word256, 0) v
    v' = step v

day6 :: IO (String, String)
day6 = do
  input <- V.fromList . map (read @Int) . words <$> (getDataDir >>= readFile . (++ "/input/input6.txt"))
  let (a, b) = runST $ V.thaw input >>= run 0 Map.empty
      finalAnsa = show a
      finalAnsb = show b
  pure (finalAnsa, finalAnsb)
