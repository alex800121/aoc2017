{-# LANGUAGE OverloadedLabels #-}

module Day22 where

import Data.Bifunctor (Bifunctor (..))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import MyLib (Direction (..), drawGraph, drawMap, toIndex)
import Optics hiding (Index)
import Paths_AOC2017

type M = Map Index [Char]

type State = (Virus, M)

type State' = (Virus, Int, (Set Index, Set Index, Set Index))

type Virus = (Index, Direction)

type Index = (Int, Int)

step :: State -> State
step ((i, d), m) = ((i', d'), m')
  where
    x = fromMaybe "." $ m Map.!? i
    (d', x') = case head x of
      '.' -> (pred d, '#' : x)
      '#' -> (succ d, '.' : x)
    i' = bimap (+ fst i) (+ snd i) $ toIndex d'
    m' = Map.insert i x' m

step' :: State' -> State'
step' ((i@(x, y), d), acc, (weakened, infected, flagged))
  | i `Set.member` weakened = ((bimap (+ x) (+ y) $ toIndex d, d), acc + 1, (Set.delete i weakened, Set.insert i infected, flagged))
  | i `Set.member` infected =
      let d' = succ d
       in ((bimap (+ x) (+ y) $ toIndex d', d'), acc, (weakened, Set.delete i infected, Set.insert i flagged))
  | i `Set.member` flagged =
      let d' = succ $ succ d
       in ((bimap (+ x) (+ y) $ toIndex d', d'), acc, (weakened, infected, Set.delete i flagged))
  | otherwise =
      let d' = pred d
       in ((bimap (+ x) (+ y) $ toIndex d', d'), acc, (Set.insert i weakened, infected, flagged))

day22 :: IO ()
day22 = do
  input <- lines <$> (getDataDir >>= readFile . (++ "/input/input22.txt"))
  -- input <- lines <$> readFile "input/test22.txt"
  let start = (length (head input) `div` 2, length input `div` 2)
      initVirus = (start, North)
      initM = drawMap (Just . (: [])) input
  -- putStrLn $ unlines $ drawGraph (\case Nothing -> '.'; Just x -> head x) $ snd $ (!! 70) $ iterate step (initVirus, initM)
  print $ sum . map (length . filter (== '#') . init) . Map.elems $ snd $ (!! 10000) $ iterate step (initVirus, initM)
  -- print $ view _2 $ (!! 100) $ iterate step' (initVirus, 0, (Set.empty, Map.keysSet $ Map.filter (== "#") initM, Set.empty))
  print $ view _2 $ (!! 10000000) $ iterate step' (initVirus, 0, (Set.empty, Map.keysSet $ Map.filter (== "#") initM, Set.empty))
