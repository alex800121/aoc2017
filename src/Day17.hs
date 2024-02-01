module Day17 where

import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import Data.Foldable (Foldable(..))

input = 335
-- input = 3

initSeq = (0, Seq.singleton 0)

step :: Int -> Int -> (Int, Seq Int) -> (Int, Seq Int)
step n i (pos, s) = (pos' + 1, s')
  where
    len = Seq.length s
    pos' = (pos + n) `mod` len
    s' = Seq.insertAt pos' i s

day17b :: Int -> Int -> Int -> Int -> [Int]
day17b st limit pos len
  | len > limit = []
  | pos' == 0 = len : next
  | otherwise = next
  where
    pos' = (pos + st) `mod` len
    next = day17b st limit (pos' + 1) (len + 1)

day17 :: IO ()
day17 = do
  print $ (\x -> let Just n = Seq.elemIndexL 2017 x in Seq.index x (n + 1)) $ snd $ foldl' (flip (step input)) initSeq [1..2017]
  print $ last $ day17b input 50000000 0 1
