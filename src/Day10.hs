module Day10 where

import Control.Lens (over, view)
import Data.Bits (Bits (..))
import Data.Char
import Data.Foldable (Foldable (..))
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import MyLib

-- input = [3, 4, 1, 5]
-- initList = Seq.fromList [0, 1, 2, 3, 4]

input = [18, 1, 0, 161, 255, 137, 254, 252, 14, 95, 165, 33, 181, 168, 2, 188]

initList = Seq.fromList [0 .. 255]

step :: Int -> (Int, KnotList) -> (Int, KnotList)
step i (n, k) = (n + 1, over focus ((`mod` len) . (+ (i + n))) k')
  where
    len = Seq.length (_list k)
    k' = twistKnotList i k


day10 :: IO ()
day10 = do
  print $ (*) <$> flip Seq.index 0 <*> flip Seq.index 1 $ view list $ snd $ foldl' (flip step) (0, KnotList 0 initList) input
  putStrLn $ knotHash $ init $ tail $ show input
