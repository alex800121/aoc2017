module Day7 where

import Paths_AOC2017
import Optics
import Control.Monad (void)
import Data.Char (isAlpha)
import Data.Function (on)
import Data.List (find, group, maximumBy, minimumBy, nub, sort, (\\), sortBy, groupBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib 
import Text.Megaparsec
import Text.Megaparsec.Char
import Debug.Trace

readInput :: Parser (Map String (Int, [String]))
readInput = do
  name <- many (satisfy isAlpha) <* space
  n <- char '(' >> signedInteger <* char ')'
  children <- fromMaybe [] <$> optional (string " -> " >> (many (satisfy isAlpha) `sepBy` string ", "))
  return $ Map.singleton name (n, children)

buildTree :: Map String (Int, [String]) -> Tree (String, Int)
buildTree m = f mother
  where
    allNodes = Map.keys m
    allChildren = Map.foldr (\(_, x) -> (x <>)) [] m
    mother = head $ allNodes \\ allChildren
    f name = Branch (name, n) children
      where
        (n, c) = m Map.! name
        children = map f c

balanceTree :: Tree (String, Int) -> Tree ((Int, Int), Bool)
balanceTree (Branch (_, i) ts) = t
  where
    s = i + sum ts''
    b = 1 >= length (nub ts'') && all (all snd) ts'
    ts' = map balanceTree ts
    ts'' = map (snd . fst . _value) ts'
    t = Branch ((i, s), b) ts'

traceFalse diff (Branch a ts) = case next of
  Just n -> traceFalse diff' n
  _ | Just x == diff' -> fst (fst (minimumBy (compare `on` snd) d)) + x
  where
    d = map ((,) <$> head <*> length) $ groupBy ((==) `on` snd) (sortBy (compare `on` snd) $ map (fst . _value) ts)
    -- diff' = max diff $ if length d >= 2 then Just $ fst (maximumBy (compare `on` snd) d) - fst (minimumBy (compare `on` snd) d) else Nothing
    next = find (not . snd . _value) ts
    x = snd (fst (maximumBy (compare `on` snd) d)) - snd (fst (minimumBy (compare `on` snd) d))
    diff' =
      if length d >= 2 && length (nub (map snd d)) > 1
        then Just x
        -- then Just (fst (maximumBy (compare `on` snd) d) - fst (minimumBy (compare `on` snd) d))
        else diff

day7 :: IO ()
day7 = do
  input <- Map.unions . mapMaybe (parseMaybe readInput) . lines <$> (getDataDir >>= readFile . (++ "/input/input7.txt"))
  -- input <- Map.unions . mapMaybe (parseMaybe readInput) . lines <$> readFile "input/test7.txt"
  let t = buildTree input
  putStrLn $ fst $ _value t
  print $ traceFalse Nothing $ balanceTree t
