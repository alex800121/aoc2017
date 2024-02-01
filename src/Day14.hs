module Day14 where

import Data.Array.Unboxed (Array)
import qualified Data.Array.Unboxed as A
import Data.Bifunctor (Bifunctor (..))
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import MyLib (drawArray, hexTo4Bits, knotHash)

type Index = (Int, Int)

input = "ffayrhll"

-- input = "flqrgnkx"
calcArray :: String -> Array Index Char
calcArray s = A.amap (\case '1' -> '#'; _ -> '.') $ drawArray $ map (\x -> let s' = s ++ '-' : show x in concat . mapMaybe hexTo4Bits $ knotHash s') [0 .. 127]

adjacent = [(0, 1), (0, -1), (1, 0), (-1, 0)]

calcAreas :: Array Index Char -> [Set Index]
calcAreas a = go [] Set.empty (Set.singleton start) a'
  where
    (start, a') = Set.deleteFindMin . Set.fromList . map fst . filter ((== '#') . snd) $ A.assocs a
    go acc acc0 next rest
      | Set.null rest = next : acc
      | Set.null next = go (acc0 : acc) Set.empty (Set.singleton next') rest'
      | otherwise = go acc (acc0 <> next) next'' rest''
      where
        (next', rest') = Set.deleteFindMin rest
        next'' = Set.unions (map (\(x, y) -> Set.map (bimap (+ x) (+ y)) next) adjacent) `Set.intersection` rest
        rest'' = rest Set.\\ next''
        
        

day14 :: IO ()
day14 = do
  -- input <- readFile "input/input14.txt"
  print $ length $ filter (== '#') $ A.elems $ calcArray input
  print $ length $ calcAreas $ calcArray input
