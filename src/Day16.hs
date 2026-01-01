module Day16 (day16) where

import Control.Arrow
import Control.Monad (foldM, foldM_)
import Control.Monad.ST.Strict (ST, runST)
import Data.Bits (Bits (..))
import Data.Char (chr, ord)
import Data.List.Split (splitOn, splitOneOf)
import Data.Tuple (swap)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word8)
import Debug.Trace
import MyLib (calcLargeCycleN, firstCycle')
import Paths_AOC2017

type V s = (STVector s Int, STVector s Int)

fromChar :: Char -> Int
fromChar = subtract (ord 'a') . ord

initV :: ST s (V s)
initV = (,) <$> MV.generate 16 fromIntegral <*> MV.generate 16 fromIntegral

n = 1000000000

readInput :: String -> ST s (V s)
readInput input = do
  v@(v0, _) <- initV
  l <- MV.generate 16 id
  offset <- foldM (f l v) 0 ls
  rotateL (offset `mod` 16) v0
  pure v
  where
    rotateL n v = do
      v' <- MV.clone v
      let (v0', v1') = MV.splitAt n v'
          (v0, v1) = MV.splitAt (16 - n) v
      MV.copy v0 v1'
      MV.copy v1 v0'
    ls = map (splitOn "/") $ splitOn "," input
    f l (v0, v1) offset =
      \case
        ['x' : a, b] -> MV.swap v0 ((offset + read a) `mod` 16) ((offset + read b) `mod` 16) >> pure offset
        ['s' : a] -> pure (offset + 16 - read a)
        ['p' : [ca], [cb]] -> do
          let ia = fromChar ca
              ib = fromChar cb
          a <- MV.read l ia
          b <- MV.read l ib
          MV.swap v1 a b
          MV.swap l ia ib
          pure offset

solidify :: V s -> ST s String
solidify (v0, v1) = do
  MV.foldrM (\i acc -> MV.read v1 i >>= \c -> pure $ chr (c + ord 'a') : acc) "" v0

danceWith :: V s -> V s -> ST s ()
danceWith i@(i0, i1) v@(v0, v1) = do
  MV.imapM_ (f v0 i0) i0
  MV.imapM_ (f v1 i1) i1
  where
    f v i a x = MV.read v x >>= MV.write i a

danceN :: Word -> V s -> V s -> ST s String
danceN !n i v
  | n <= 0 = solidify i
  | n `testBit` 0 = do
      vc <- clonePair v
      danceWith i vc
      danceWith v vc
      danceN (n `shiftR` 1) i v
  | otherwise = do
      vc <- clonePair v
      danceWith v vc
      danceN (n `shiftR` 1) i v

clonePair (v0, v1) = (,) <$> MV.clone v0 <*> MV.clone v1

day16 :: IO (String, String)
day16 = do
  input <- init <$> (getDataDir >>= readFile . (++ "/input/input16.txt"))
  let
    finalAnsa = runST (readInput input >>= solidify)
  let
    finalAnsb = runST (initV >>= \i -> readInput input >>= danceN n i)
  pure (finalAnsa, finalAnsb)
