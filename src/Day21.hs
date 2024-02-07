module Day21 where

import Data.Array.IArray (Array)
import qualified Data.Array.IArray as A
import Data.Bifunctor (Bifunctor (..))
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Debug.Trace (traceShow)
import MyLib (drawArray, drawGraph)

type Index = (Int, Int)

type Rule = Map M M

type Rule33 = Map M (Map M (Set Index))

type M = Array Index Char

initArray = drawArray @Array [".#.", "..#", "###"]

convertRule :: Rule -> Rule33
convertRule r = r33
  where
    k = Map.keys r
    r3 = filter ((== 9) . length) k
    r33 = Map.fromList $ map ((,) <$> id <*> splitArray' 3 . step' r . step' r . step' r) r3

printArray :: M -> String
printArray = unlines . drawGraph (fromMaybe ' ') . Map.fromList . A.assocs

flipArray :: M -> M
flipArray m = A.ixmap b' f m
  where
    ((x0, y0), (x1, y1)) = A.bounds m
    b' = ((0, 0), (x1 - x0, y1 - y0))
    f (x, y) = (x1 - x, y - y0)

joinArray' :: Map M (Set Index) -> M
joinArray' m = joinArray $ A.array b m'
  where
    m' = do
      (x, y) <- Map.toList m
      i <- Set.toList y
      pure (i, x)
    b = ((,) <$> minimum <*> maximum) $ map fst m'

joinArray :: Array Index M -> M
joinArray a0 =
  {- traceShow (a0, b1, b) $  -}
  A.array
    b
    [ ((x, y), i)
      | -- [traceShow ((x, y), i) ((x, y), i)
        xy0 <- A.range b0,
        xy1@(x1, y1) <- A.range b1,
        let (x, y) = bimap (+ x1) (+ y1) $ f xy0,
        let i = (a0 A.! xy0) A.! xy1
    ]
  where
    b0 = A.bounds a0
    a1 = a0 A.! fst b0
    b1@((x0, y0), (x1, y1)) = A.bounds a1
    f = bimap (* (x1 - x0 + 1)) (* (y1 - y0 + 1))
    b = bimap (bimap (+ x0) (+ y0) . f) (bimap (+ x1) (+ y1) . f) b0

step' r = joinArray . step r

-- step :: Rule -> M -> M
step r a
  | even l = A.amap (r Map.!) $ splitArray 2 a
  | l `mod` 3 == 0 = A.amap (r Map.!) $ splitArray 3 a
  where
    b@((x0, y0), (x1, y1)) = A.bounds a
    l = x1 - x0 + 1

splitArray' :: Int -> M -> Map M (Set Index)
splitArray' n = go . splitArray n
  where
    go = Map.unionsWith Set.union . map (uncurry Map.singleton . fmap Set.singleton . swap) . A.assocs

splitArray :: Int -> M -> Array Index M
splitArray n ax =
  A.array @Array
    -- traceShow (n, b0, b1, b) $ A.array @Array
    b0
    [ ((x0', y0'), a')
      | (x0', y0') <- A.range b0,
        let a' =
              A.array @Array
                b1
                [ ((x1', y1'), i)
                  | (x1', y1') <- A.range b1,
                    let i = ax A.! ((x0' * n) + x1' + x0, (y0' * n) + y1' + y0)
                ]
    ]
  where
    b@((x0, y0), (x1, y1)) = A.bounds ax
    l = x1 - x0 + 1
    b0 = ((x0, y0), ((l `div` n) - 1 + x0, (l `div` n) - 1 + y0))
    b1 = ((0, 0), (n - 1, n - 1))

rotateArray :: M -> M
rotateArray m = A.ixmap b f m'
  where
    ((x0, y0), (x1, y1)) = A.bounds m
    m' = A.ixmap ((0, 0), (x1 - x0, y1 - y0)) (bimap (+ x0) (+ y0)) m
    b@((x0', y0'), (x1', y1')) = A.bounds m'
    f (x, y) = (y, negate (x - x1' + x0'))

readInput :: String -> Rule
readInput s = Map.fromList $ do
  f0 <-
    [ flipArray,
      flipArray . flipArray
      ]
  f1 <-
    [ rotateArray,
      rotateArray . rotateArray,
      rotateArray . rotateArray . rotateArray,
      rotateArray . rotateArray . rotateArray . rotateArray
      ]
  let f = f0 . f1
  pure (f a, b)
  where
    [a, b] = map (drawArray @Array . splitOn "/") $ splitOn " => " s

skip33 :: Rule33 -> Map M (Set Index) -> Map M (Set Index)
skip33 r33 m = Map.unionsWith Set.union x
  where
    x = do
      (a, b) <- Map.toList m
      (a', b') <- Map.toList $ r33 Map.! a
      let b'' = Set.unions $ Set.map (\(x, y) -> Set.map (bimap ((+ x) . (* 3)) ((+ y) . (* 3))) b) b'
      pure $ Map.singleton a' b''

quick :: Rule -> Rule33 -> Int -> Map M (Set Index) -> Map M (Set Index)
quick r0 r33 n ax
  | n <= 0 = ax
  -- \| even l = quick r0 r33 (n - 1) (step' r0 ax)
  | l `mod` 3 == 0 && n >= 3 = go n ax
  where
    b@((x0, y0), (x1, y1)) = A.bounds $ head $ Map.keys ax
    l = x1 - x0 + 1
    n' = sum $ Map.map length ax
    go n x
      | n < 3 = quick r0 r33 n x
      | otherwise = go (n - 3) (skip33 r33 x)

day21 :: IO ()
day21 = do
  input <- Map.unions . map readInput . lines <$> readFile "input/input21.txt"
  print $ length $ filter (== '#') $ printArray $ (!! 5) $ iterate (step' input) initArray
  print
    . sum
    . map (\(x, y) -> length (filter (== '#') $ A.elems x) * length y)
    . Map.toList
    $ quick input (convertRule input) 18 (splitArray' 3 initArray)
