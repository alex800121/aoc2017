{-# LANGUAGE OverloadedLabels #-}

module Day10 where

import Control.Monad.ST.Strict (runST)
import Data.Bits (Bits (..))
import Data.Char
import Data.Foldable (Foldable (..))
import Data.List (foldl')
import Data.List.Split (chunksOf, splitOn)
import Data.Sequence (Seq (..))
import Data.Sequence qualified as Seq
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as MUV
import Debug.Trace
import MyLib
import Optics
import Paths_AOC2017
import Control.Monad (foldM)

initList = Seq.fromList [0 .. 255]

step :: Int -> (Int, KnotList) -> (Int, KnotList)
step i (n, k) = (n + 1, over #_focus ((`mod` len) . (+ (i + n))) k')
  where
    len = Seq.length (_list k)
    k' = twistKnotList i k

day10 :: IO (String, String)
day10 = do
  input <- map read . splitOn "," <$> (getDataDir >>= readFile . (++ "/input/input10.txt"))
  let
    finalAnsa =
      show $ runST $ do
        let len = 256
        v0 <- MUV.generate len id
        v1 <- MUV.generate len id
        let f ((a, b), f0) (skip, i) = do
              stepSTKL len a b f0 i
              pure ((b, a), (f0 + i + skip) `mod` len)
        ((a, _), _) <- foldM f ((v0, v1), 0) (zip [0..] input)
        (*) <$> MUV.read a 0 <*> MUV.read a 1
  let
    finalAnsb =
      knotHash' $ init $ tail $ show input
  pure (finalAnsa, finalAnsb)
