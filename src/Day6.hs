module Day6 where

import Paths_AOC2017
import Paths_AOC2017
import Data.List (group, sort)
import Data.Vector (Vector)
import qualified Data.Vector as V
import MyLib (firstRepeat', firstCycle')

step :: Vector Int -> Vector Int
step v = v''
  where
    l = V.length v
    i = V.maxIndex v
    m = v V.! i
    v' = v V.// [(i, 0)]
    l' =
      [ (x, y')
        | (x, y) <- ((,) <$> head <*> length) <$> group (sort $ map (`mod` l) [i + 1 .. i + m]),
          let y' = y + v' V.! x
      ]
    v'' = v' V.// l'

day6 :: IO ()
day6 = do
  input <- V.fromList . map (read @Int) . words <$> (getDataDir >>= readFile . (++ "/input/input6.txt"))
  -- input <- V.fromList . map (read @Int) . words <$> readFile "input/test6.txt"
  let a = firstCycle' $ iterate step input
  print $ fmap (\(x, y, _) -> x + y) a
  print $ fmap (\(x, _, _) -> x) a
