module Day21 (day21) where

import Control.Monad.ST.Strict (ST, runST)
import Data.Array.IArray qualified as IA
import Data.Array.ST (STUArray)
import Data.Bits (Bits (..))
import Data.Foldable (foldrM)
import Data.Function (fix)
import Data.IntMultiSet qualified as MS
import Data.List (partition, transpose, unfoldr)
import Data.List.Split (chunksOf, condense, dropFinalBlank, split, splitOn, whenElt)
import Data.Vector qualified as SV
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV
import Paths_AOC2017

readInput input = (g 2 a, g 3 b)
  where
    (a, b) =
      partition ((== 2) . length . head)
        . map (map (splitOn "/") . splitOn " => ")
        $ lines input
    g :: Int -> [[[String]]] -> V.Vector (Int, Int)
    g n xs = runST $ do
      v <- MV.new (2 ^ (n * n))
      mapM_ (\[a, b] -> mapM_ (\a -> MV.write v (toInt '#' a) (n + 1, toInt '#' b)) (f a)) xs
      V.freeze v
    f x =
      [ a . b . c $ x
      | a <- [id, reverse]
      , b <- [id, map reverse]
      , c <- [id, transpose]
      ]

toInt b = foldl' (\acc x -> if x == b then acc * 2 + 1 else acc * 2) 0 . concat
fromInt c0 c1 (n, x) =
  chunksOf n . reverse $
    unfoldr
      ( \(m, b) ->
          if
            | m <= 0 -> Nothing
            | testBit b 0 -> Just (c0, (m - 1, b `shiftR` 1))
            | otherwise -> Just (c1, (m - 1, b `shiftR` 1))
      )
      (n * n, x)

type Index = (Int, Int)

test =
  [ [True, True, True, False, True, True]
  , [False, True, False, True, False, False]
  , [True, False, False, True, False, True]
  , [False, False, False, False, True, False]
  , [False, False, False, False, True, False]
  , [False, False, False, False, True, False]
  ]

step m23 m34 ll
  | testBit len 0 = f m34 3
  | otherwise = f m23 2
  where
    len = length ll
    f m n =
      concatMap
        ( map concat
            . transpose
            . map
              ( fromInt True False
                  . (m V.!)
                  . toInt True
              )
            . transpose
            . map (chunksOf n)
        )
        $ chunksOf n ll

initV =
  map (map (== '#')) $
    lines
      """
      .#.
      ..#
      ###"""

calc3 m23 m34 ll = n3
  where
    ll3 = iterate (step m23 m34) ll !! 3
    n = toInt True ll
    n3 = MS.fromList . concatMap (map (toInt True) . transpose . map (chunksOf 3)) $ chunksOf 3 ll3

calcAll m23 m34 = SV.generate (2 ^ 9) (calc3 m23 m34 . fromInt True False . (3,))

day21 :: IO (String, String)
day21 = do
  (m23, m34) <- readInput <$> (getDataDir >>= readFile . (++ "/input/input21.txt"))
  let
    step3 = calcAll m23 m34
    finalAnsa =
      show
        . length
        . concatMap (filter id)
        $ iterate (step m23 m34) initV !! 5
    finalAnsb =
      show
        . MS.foldOccur (\k o -> ((o * popCount k) +)) 0
        $ iterate (`MS.bind` (step3 SV.!)) (MS.singleton (toInt True initV)) !! (18 `div` 3)
  pure (finalAnsa, finalAnsb)
