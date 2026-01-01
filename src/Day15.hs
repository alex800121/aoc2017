module Day15 (day15) where

import Control.Arrow
import Control.Concurrent
import Control.Monad (foldM, replicateM)
import Control.Parallel (par)
import Control.Parallel.Strategies
import Data.Bits
import Data.Char (intToDigit, isDigit)
import Data.Word (Word64)
import Numeric (showIntAtBase)
import Paths_AOC2017

ga = 16807

gb = 48271

-- x = n * (2 ^ 31) + m
--   = n * (2 ^ 31 - 1) + n + m
-- m(2 ^ 31 - 1) = (d(2 ^ 31) + m(2 ^ 31)) `mod` (2 ^ 32 - 1)

d = 2147483647 :: Word64
m31 = bit 31 - 1 :: Word64

fastMod n
  | x `shiftR` 31 == 0 = x
  | otherwise = x - m31
  where
    !d = n `shiftR` 31
    !m = n .&. m31
    !x = d + m

na = 40000000

modExpo x = fastMod . (* x)

-- 5 ^ 13 % 11
-- 5 ^ (1 + 4 + 8) % 11
--
modExpoN b = go 1 (fastMod b)
  where
    go acc b x
      | x > 0 = go (if testBit x 0 then fastMod (acc * b) else acc) (fastMod (b * b)) (x `shiftR` 1)
      | otherwise = acc

nb = 5000000

fb !b !x !n
  | n' .&. b == 0 = n'
  | otherwise = fb b x n'
  where
    !n' = modExpo x n

day15a :: (Word64 -> Word64 -> Word64) -> (Word64 -> Word64 -> Word64) -> Int -> Word64 -> Word64 -> Word64 -> Word64
day15a fa fb !na !acc !a !b
  | na < 0 = acc
  | ma == mb = day15a fa fb (na - 1) (acc + 1) a' b'
  | otherwise = day15a fa fb (na - 1) acc a' b'
  where
    !a' = fa ga a
    !b' = fb gb b
    ma = a' .&. (bit 16 - 1)
    mb = b' .&. (bit 16 - 1)

threads = 8

day15 :: IO (String, String)
day15 = do
  !a : !b : _ <- map ((read @Word64) . filter isDigit) . lines <$> (getDataDir >>= readFile . (++ "/input/input15.txt"))
  let
    finalAnsb = show $ day15a (fb 3) (fb 7) nb 0 a b
    finalAnsa =
      par finalAnsb
        $ show
          . sum
          . parMap rpar (uncurry (day15a modExpo modExpo (na `div` threads) 0))
        $ [ (a0, b0)
          | n <- [0, (na `div` threads) .. na - 1]
          , let !a0 = fastMod $ a * modExpoN ga n
          , let !b0 = fastMod $ b * modExpoN gb n
          ]
  pure (finalAnsa, finalAnsb)
