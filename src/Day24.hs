module Day24 where

import Control.Applicative ((<|>))
import Control.Arrow
import Control.Monad (guard)
import Control.Monad.ST.Strict (runST)
import Data.Bits (Bits (..))
import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.IntMap.Lazy qualified as IM
import Data.IntSet qualified as IS
import Data.List (delete, maximumBy, sort, sortBy, unfoldr, (\\))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV
import Debug.Trace (traceShowM)
import MyLib (pickAnySplit)
import Paths_AOC2017

buildBridge c p
  | IS.null next = [(0, 0)]
  | p `IS.member` next =
      [ (a + 2, b + 2 * p)
      | let c' = IM.adjust (IS.delete p) p c
      , (!a, !b) <- buildBridge c' p
      ]
  | otherwise =
      [ (a + 2, b + p + x)
      | !x <- IS.toList next
      , let c' = IM.adjust (IS.delete p) x $ IM.adjust (IS.delete x) p c
      , (!a, !b) <- buildBridge c' x
      ]
  where
    next = fromMaybe IS.empty (c IM.!? p)

buildBridge' c p
  | next == 0 = [(0, 0)]
  | testBit next p =
      [ (a + 2, b + 2 * p)
      | let c' = V.modify (\v -> MV.modify v (`clearBit` p) p) c
      , (!a, !b) <- buildBridge' c' p
      ]
  | otherwise =
      [ (a + 2, b + p + x)
      | !x <- toIL next
      , let c' = V.modify (\v -> MV.modify v (`clearBit` x) p >> MV.modify v (`clearBit` p) x) c
      , (!a, !b) <- buildBridge' c' x
      ]
  where
    next = c V.! p

toIM = foldl' @[] f IM.empty
  where
    f acc [a, b] = IM.insertWith (<>) a (IS.singleton b) $ IM.insertWith (<>) b (IS.singleton a) acc

toIL :: Int -> [Int]
toIL = go 0
  where
    go n x
      | x == 0 = []
      | testBit x 0 = n : go (n + 1) (x `shiftR` 1)
      | otherwise = go (n + 1) (x `shiftR` 1)

toV xs = runST $ do
  v <- MV.replicate m (0 :: Int)
  mapM_ (\[a, b] -> MV.modify v (`setBit` a) b >> MV.modify v (`setBit` b) a) xs
  V.freeze v
  where
    m = 1 + maximum (concat xs)

day24 :: IO (String, String)
day24 = do
  input <- map (map (read @Int) . splitOn "/") . lines <$> (getDataDir >>= readFile . (++ "/input/input24.txt"))
  let b = buildBridge' (toV input) 0
  -- let b = buildBridge (toIM input) 0
  let
    finalAnsa =
      show
        . maximum
        $ map snd b
  let
    finalAnsb =
      show
        . snd
        . maximumBy (\a b -> (compare `on` fst) a b <> (compare `on` snd) a b)
        $ b
  pure (finalAnsa, finalAnsb)
