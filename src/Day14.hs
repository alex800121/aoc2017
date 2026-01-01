module Day14 where

import Control.Parallel.Strategies
import Data.Bits (Bits (..))
import Data.DisjointSet
import Data.Maybe (fromMaybe)
import Data.Vector.Storable qualified as V
import Data.WideWord (Word128)
import Paths_AOC2017
import MyLib (knotHashWord')

type Index = (Int, Int)

calcArray :: String -> V.Vector Word128
calcArray s = V.fromList $ parMap rpar (\x -> let s' = s ++ '-' : show x in knotHashWord' s') [0 .. 127]

calcArea :: V.Vector Word128 -> DisjointSet (Int, Int)
calcArea v = V.ifoldl' f empty v
  where
    f !acc !i = go 0 (fromMaybe 0 (v V.!? (i - 1))) acc i
    go !j !p !acc !i !x
      | x == 0 = acc
      | x `testBit` 0 = go (j + 1) (p `shiftR` 1) acc'' i (x `shiftR` 1)
      | otherwise = go (j + 1) (p `shiftR` 1) acc i (x `shiftR` 1)
      where
        acc' = if x `testBit` 1 then union (i, j) (i, j + 1) acc else insert (i, j) acc
        acc'' = if p `testBit` 0 then union (i, j) (i - 1, j) acc' else acc'

day14 :: IO (String, String)
day14 = do
  input <- init <$> (getDataDir >>= readFile . (++ "/input/input14.txt"))
  let
    !v = calcArray input
    finalAnsa =
      show . sum . map popCount . V.toList $ v
  let
    -- finalAnsb = ""
    finalAnsb = show $ sets $ calcArea v
  pure (finalAnsa, finalAnsb)
