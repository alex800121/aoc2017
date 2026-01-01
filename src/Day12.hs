module Day12 where

import Paths_AOC2017
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib (Parser, signedInteger)
import Text.Megaparsec
import Text.Megaparsec.Char

type Pipe = IntMap (Set Int)

readInput :: Parser Pipe
readInput =
  IM.singleton
    <$> (signedInteger <* string " <-> ")
    <*> (Set.fromList <$> (signedInteger `sepBy` string ", "))

groupPipe :: Int -> Pipe -> Set Int
groupPipe n p = go Set.empty (Set.singleton n)
  where
    go acc next
      | Set.null next = acc
      | otherwise = go acc' next'
      where
        next' = Set.unions (Set.map (p IM.!) next) Set.\\ acc'
        acc' = Set.union acc next

pipeGroups :: Pipe -> [Set Int]
pipeGroups p
  | IM.null p = []
  | otherwise = s : pipeGroups p'
  where
    s = groupPipe (fst $ IM.findMin p) p
    p' = IM.mapMaybeWithKey (\k a -> if k `Set.member` s then Nothing else Just (a Set.\\ s)) p

day12 :: IO (String, String)
day12 = do
  input <- IM.unions . mapMaybe (parseMaybe readInput) . lines <$> (getDataDir >>= readFile . (++ "/input/input12.txt"))
  -- input <- IM.unions . mapMaybe (parseMaybe readInput) . lines <$> readFile "input/test12.txt"
  let
   finalAnsa
    = show $ length $ groupPipe 0 input
  
  let
   finalAnsb
    = show $ length $ pipeGroups input
  pure (finalAnsa, finalAnsb)
