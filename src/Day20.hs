module Day20 where

import Data.List (elemIndex, findIndex)
import Data.List.Split (divvy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified GHC.IsList as IL
import MyLib (Nat (..), Parser, SNat (..), Vec (..), pick, signedInteger, vRead, vZipWith)
import Text.Megaparsec
import Text.Megaparsec.Char

type Star a = V3 a

type Star' = Star (V3 Int)

type V3 a = Vec S3 a

type V2 a = Vec S3 a

type S2 = S (S Z)

type S3 = S S2

-- p=<-3053,-52,-1564>, v=<58,-53,52>, a=<8,5,2>
starParser :: Parser Star'
starParser = do
  string "p=<"
  p <- IL.fromList <$> (signedInteger `sepBy` char ',')
  string ">, v=<"
  v <- IL.fromList <$> (signedInteger `sepBy` char ',')
  string ">, a=<"
  a <- IL.fromList <$> (signedInteger `sepBy` char ',')
  char '>'
  pure $ Cons a (Cons v (Cons p Nil))

diverge :: Star' -> Star' -> Int
diverge s0 s1 = length $ takeWhile ((>) <$> head <*> last) $ divvy 2 1 m
  where
    m0 = map ((`vRead` 2) . (`moveStar` s0)) [0 ..]
    m1 = map ((`vRead` 2) . (`moveStar` s1)) [0 ..]
    m = zipWith (\a b -> sum $ abs (a - b)) m0 m1

moveStar :: Int -> Star' -> Star'
moveStar n (Cons a (Cons v (Cons x Nil))) =
  Cons
    a
    ( Cons
        (v + a * n')
        ( Cons
            (x + (v * n') + (a * fmap (`div` 2) ((n' * n') + n')))
            Nil
        )
    )
  where
    n' = pure n

{-

t0 = a v x
t1 = a (v + a * 1) (x + (v + a * 1))
t2 = a (v + a * 2) (x + (v + a * 1) + (v + (a * 2)))
t3 = a (v + a * 3) (x + (v + a * 1) + (v + (a * 2)) + (v + (a * 3)))
...
tn = a (v + a * n) (x + (v * n) + (a * ((n * (n + 1)) `div` 2)))

-}

day20b :: [Star'] -> [Star']
day20b input = go allDiverge input
  where
    allDiverge = maximum $ map (\[x, y] -> diverge x y) $ pick 2 input
    go n s
      | n < 0 = s
      | otherwise = go (n - 1) s''
      where
        s' = map (moveStar 1) s
        s'' =
          Map.elems
            . fst
            $ foldr f (Map.empty, Set.empty) s'
        f x (acc, collided)
          | x' `Set.member` collided = (Map.delete x' acc, collided)
          | x' `Map.member` acc = (Map.delete x' acc, Set.insert x' collided)
          | otherwise = (Map.insert x' x acc, Set.insert x' collided)
          where
            x' = vRead x 2

day20 :: IO ()
day20 = do
  input <- mapMaybe (parseMaybe starParser) . lines <$> readFile "input/input20.txt"
  -- input <- mapMaybe (parseMaybe starParser) . lines <$> readFile "input/test20.txt"
  let a = map (sum . fmap abs . (`vRead` 0)) input
      m = minimum a
  print $ elemIndex m a
  print $ length $ day20b input
