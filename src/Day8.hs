module Day8 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Foldable (Foldable(..))
import Data.Bifunctor (Bifunctor(..))

readRule :: String -> Map String Int -> Map String Int
readRule s m = Map.insertWith (+) r0 (if rule (fromMaybe 0 (m Map.!? r1)) then ins else 0) m
  where
    [r0, ins', n0', _, r1, rule', n1'] = words s
    ins = case ins' of
      "dec" -> negate (read @Int n0')
      "inc" -> read @Int n0'
    rule =
      flip
        ( case rule' of
            "==" -> (==)
            "!=" -> (/=)
            ">=" -> (>=)
            "<=" -> (<=)
            ">" -> (>)
            "<" -> (<)
        )
        (read @Int n1')

day8 :: IO ()
day8 = do
  input <- lines <$> readFile "input/input8.txt"
  -- input <- lines <$> readFile "input/test8.txt"
  let (a, b) = first maximum $ foldl' (\(m, n) x -> let m' = readRule x m in (m', max n (maximum m'))) (Map.empty, 0) input
  print a
  print b
