{-
 - Adapted from
 - https://github.com/maneatingape/advent-of-code-rust/blob/main/src/year2017/day22.rs
 -}

module Day22 (day22) where

import Control.Monad (when)
import Control.Monad.ST.Strict (ST, runST)
import Data.Array.Unboxed qualified as UA
import Data.Bits (Bits (..))
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable (STVector)
import Data.Vector.Unboxed.Mutable qualified as MV
import Data.Word (Word64)
import Debug.Trace (traceShow, traceShowM)
import MyLib (drawArray)
import Paths_AOC2017

size = 500

fromInt = (`divMod` (size `div` 2))

toInt (x, y) = x * (size `div` 2) + y

dir 0 = (0, -1)
dir 1 = (1, 0)
dir 2 = (0, 1)
dir 3 = (-1, 0)
dir 4 = (0, 0)

readInput ls = v1
  where
    len = length ls
    a0 = drawArray @UA.Array ls
    v1 = V.generate ((size `div` 2) ^ 2) g
    g = f . fromInt
    f (x, y)
      | xd >= 0 && yd >= 0 = foldl' g 0 [(xd2 + 1, yd2 + 1), (xd2, yd2 + 1), (xd2 + 1, yd2), (xd2, yd2)]
      | otherwise = 0 :: Int
      where
        xd = x - size `div` 4 + len `div` 4
        yd = y - size `div` 4 + len `div` 4
        xd2 = xd * 2
        yd2 = yd * 2
        g acc x = case a0 UA.!? x of
          Just '#' -> acc * 4 + 2
          _ -> acc * 4

-- 0-7 q this square
-- 8-9 j next quadrant
-- 10-11 d next direction
-- 12-15 s steps required
-- 16-19 acc infection added
-- 20- i next index

toIndex q j d =
  q
    .|. j `shiftL` 8
    .|. d `shiftL` 10
toCache q j d s acc i =
  toIndex q j d
    .|. s `shiftL` 12
    .|. acc `shiftL` 16
    .|. i `shiftL` 20
fromCache n =
  ( n `mod` bit 8
  , n `shiftR` 8 `mod` bit 2
  , n `shiftR` 10 `mod` bit 2
  , n `shiftR` 12 `mod` bit 4
  , n `shiftR` 16 `mod` bit 4
  , n `shiftR` 20
  )

calcCache f = go 0 0
  where
    go step acc q j d
      | i' /= 4 = toCache q' j' d' step' acc' i'
      | otherwise = go step' acc' q' j' d'
      where
        b = readQ q j
        (d', b') = f d b
        acc' = if b' == 2 then acc + 1 else acc
        step' = step + 1
        (i', j') = next j d'
        q' = writeQ q j b b'

qs = map (f 0 4) [0 .. 15]
  where
    f acc 0 _ = acc
    f acc n x
      | testBit x 0 = f (acc * 4 + 2) (n - 1) (x `shiftR` 1)
      | otherwise = f (acc * 4) (n - 1) (x `shiftR` 1)

cacheA = V.replicate 4096 0 V.// [(toIndex q j d, calcCache fa q j d) | q <- qs, j <- [0 .. 3], d <- [0 .. 3]]

cacheB = V.replicate 4096 0 V.// [(toIndex q j d, calcCache fb q j d) | q <- [0 .. bit 8 - 1], j <- [0 .. 3], d <- [0 .. 3]]

step f !n !i !q !j !d !acc !cache v
  -- | traceShow (n, fromInt i, q, j, d, acc, fromCache $ cache V.! toIndex q j d) False = undefined
  | n > 8 = do
      let (q', j', d', s', acc', i') = fromCache $ cache V.! toIndex q j d
      MV.write v i q'
      q'' <- MV.read v (i + toInt (dir i'))
      step f (n - s') (i + toInt (dir i')) q'' j' d' (acc + acc') cache v
  | n <= 0 = pure acc
  | otherwise = do
      b <- readV v i j
      let (d', b') = f d b
          acc' = if b' == 2 then acc + 1 else acc
          (i', j') = next j d'
      writeV v i j b b'
      step f (n - 1) (i + toInt (dir i')) q j' d' acc' cache v

right = (`mod` 4) . succ
left = (`mod` 4) . pred
back = (`mod` 4) . succ . succ

fa d 0 = (left d, 2)
fa d 2 = (right d, 0)

fb d 0 = (left d, 1)
fb d 1 = (d, 2)
fb d 2 = (right d, 3)
fb d 3 = (back d, 0)

writeQ q j b b' = b' `shiftL` j2 .|. q `xor` (b `shiftL` j2)
  where
    j2 = j * 2

readQ q j = q `shiftR` (j * 2) `mod` 4
readV v i j = (`mod` 4) . (`shiftR` (j * 2)) <$> MV.read v i
writeV v i j b b' = MV.modify v ((.|. (b' `shiftL` j2)) . (`xor` (b `shiftL` j2))) i
  where
    j2 = j * 2

--    0
--   0-1
-- 3 | | 1
--   2-3
--    2

next 0 0 = (0, 2)
next 0 3 = (3, 1)
next 1 0 = (0, 3)
next 1 1 = (1, 0)
next 2 3 = (3, 3)
next 2 2 = (2, 0)
next 3 2 = (2, 1)
next 3 1 = (1, 2)
next 0 1 = (4, 1)
next 0 2 = (4, 2)
next 1 3 = (4, 0)
next 1 2 = (4, 3)
next 2 0 = (4, 0)
next 2 1 = (4, 3)
next 3 0 = (4, 1)
next 3 3 = (4, 2)

na = 10000 :: Int

nb = 10000000 :: Int

day22 :: IO (String, String)
day22 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input22.txt"))
  let
    aQuad = readInput input
    len = aQuad
    start = toInt (size `div` 4, size `div` 4)
    q0 = aQuad V.! start
    finalAnsa = show $ runST $ step fa na start q0 0 0 0 cacheA =<< V.thaw aQuad
    finalAnsb = show $ runST $ step fb nb start q0 0 0 0 cacheB =<< V.thaw aQuad
  pure (finalAnsa, finalAnsb)
