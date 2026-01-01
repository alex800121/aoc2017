module Day20 (day20) where

import Control.Parallel.Strategies
import Data.Bits (Bits (..))
import Data.Containers.ListUtils (nubInt, nubOrd)
import Data.Function (on)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (elemIndex, foldl1', groupBy, sort, sortBy, tails)
import Data.List.Split (divvy)
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Data.WideWord (Word256)
import Debug.Trace (traceShow)
import MyLib (Parser, signedInteger)
import Optics
import Paths_AOC2017
import Text.Megaparsec (parseMaybe, sepBy)
import Text.Megaparsec.Char (char, string)
import Data.IntSet (IntSet)

type Star' = [[Int]]

-- p=<-3053,-52,-1564>, v=<58,-53,52>, a=<8,5,2>
starParser :: Parser Star'
starParser = do
  string "p=<"
  p <- signedInteger `sepBy` char ','
  string ">, v=<"
  v <- signedInteger `sepBy` char ','
  string ">, a=<"
  a <- signedInteger `sepBy` char ','
  char '>'
  pure [p, v, a]

moveStar :: Int -> Star' -> [Int]
moveStar n [x, v, a] =
  foldl1' (zipWith (+)) [x, map (* n) v, map (* (n * (n + 1) `div` 2)) a]

{-
 - f(t) = p0 + (v0 * t) + (a0 * ((t * (t + 1)) `div` 2))
 - 2 * dp + 2 * dv * n + da * t^2 + da * t = 0
 - da * t^2 + (2 * dv + da) * t + 2 * dp = 0
 - if a /= 0 -> t = (- 2 * dv - da +/- sqrt((2 * dv + da)^2 - 4 * da * 2 * dp) ) / (2 * da)
 - if a == 0 -> t = -dp / dv
 -}

calcColl
  (i0, [[px0, py0, pz0], [vx0, vy0, vz0], [ax0, ay0, az0]])
  (i1, [[px1, py1, pz1], [vx1, vy1, vz1], [ax1, ay1, az1]])
    | a /= 0
    , Just r <- findSqrt s
    , t : _ <-
        sort
          [ d
          | r' <- [r, -r]
          , let num = -b + r'
          , let den = 2 * a
          , let (d, m) = num `divMod` den
          , m == 0
          , d >= 0
          , py0 + (vy0 * d) + (ay0 * ((d * (d + 1)) `div` 2))
              == py1 + (vy1 * d) + (ay1 * ((d * (d + 1)) `div` 2))
          ] =
        Just (t, [i0, i1])
    | dvx /= 0
    , (d, m) <- (-dpx) `divMod` dvx
    , m == 0
    , d >= 0
    , py0 + (vy0 * d) + (ay0 * ((d * (d + 1)) `div` 2))
        == py1 + (vy1 * d) + (ay1 * ((d * (d + 1)) `div` 2)) =
        Just (d, [i0, i1])
    | dvx == 0, px0 == px1, py0 == py1, pz0 == pz1 = Just (0, [i0, i1])
    | otherwise = Nothing
    where
      dpx = px0 - px1
      dvx = vx0 - vx1
      dax = ax0 - ax1
      a = dax
      b = 2 * dvx + dax
      c = 2 * dpx
      s = b * b - 4 * a * c

findSqrt n = go 0 2
  where
    go lower upper
      | u2 == n = Just upper
      | l2 == n = Just lower
      | m2 == n = Just m
      | lower + 1 == upper = Nothing
      | u2 < n = go lower (upper * 2)
      | m2 < n = go m upper
      | l2 < n = go lower m
      | otherwise = Nothing
      where
        l2 = lower * lower
        u2 = upper * upper
        m = (lower + upper) `div` 2
        m2 = m * m

{-

t0 = a v x
t1 = a (v + a * 1) (x + (v + a * 1))
t2 = a (v + a * 2) (x + (v + a * 1) + (v + (a * 2)))
t3 = a (v + a * 3) (x + (v + a * 1) + (v + (a * 2)) + (v + (a * 3)))
...
tn = a (v + a * n) (x + (v * n) + (a * ((n * (n + 1)) `div` 2)))

-}

type Count = (Word256, Word256, Word256, Word256)

day20b :: [[(Int, [Int])]] -> IntSet
day20b = foldl' f IS.empty
  where
    f !n =
      (n <>)
        . foldl' (g n) IS.empty
    g n !acc (_, [i0, i1])
      | i0 `IS.member` n || i1 `IS.member` n = acc
      | otherwise = IS.insert i0 $ IS.insert i1 acc

collided (n0, n1, n2, n3) i =
  i < 256 && testBit n0 i
    || i < 512 && testBit n1 (i - 256)
    || i < 768 && testBit n2 (i - 512)
    || testBit n3 (i - 768)

addColl i (n0, n1, n2, n3)
  | i < 256 = (setBit n0 i, n1, n2, n3)
  | i < 512 = (n0, setBit n1 (i - 256), n2, n3)
  | i < 768 = (n0, n1, setBit n2 (i - 512), n3)
  | otherwise = (n0, n1, n2, setBit n3 (i - 768))

calcAll :: (Word256, Word256, Word256, Word256) -> Sum Int
calcAll = foldMapOf each (Sum . popCount)

tick n = go 0
  where
    go t stars
      | t >= n = stars
      | otherwise =
          go (t + 1)
            . concatMap (map snd)
            . filter ((<= 1) . length)
            . groupBy ((==) `on` fst)
            . sort
            $ [(h', xs) | xs <- stars, let h' = moveStar t xs]

day20 :: IO (String, String)
day20 = do
  input <- mapMaybe (parseMaybe starParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  -- input <- zip [0 ..] . mapMaybe (parseMaybe starParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input20.txt"))
  let a = zip (map (map (sum . map abs) . reverse) input) [0..]
      !ts =
        nubInt
          . sort
          $ mapMaybe (fmap fst . uncurry calcColl) [(a, b) | a : xs <- tails (zip [0 ..] input), b <- xs]
      finalAnsa = show . snd $ minimum a
      finalAnsb = show . length $ tick (maximum ts) input
  pure (finalAnsa, finalAnsb)
