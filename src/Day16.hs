module Day16 where

import Data.Char (chr, ord)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Tuple (swap)
import MyLib (firstCycle', calcLargeCycleN)

type Pos = (IntMap Int, IntMap Int)

fromChar :: Char -> Int
fromChar = subtract (ord 'a') . ord

initPos = IM.fromList [(x, x) | x <- [0 .. 15]]

initPos' = (initPos, initPos)

-- initPos = IM.fromList [(x, x) | x <- [0 .. 4]]

n = 1000000000

fromPos :: Pos -> String
fromPos = map (chr . (+ ord 'a')) . IM.elems . snd

readIns :: Int -> String -> Pos -> Pos
readIns l ('s' : xs) (chrPos, posChr) = (IM.map ((`mod` l) . (+ read @Int xs)) chrPos, IM.mapKeys ((`mod` l) . (+ read @Int xs)) posChr)
readIns l ('x' : xs) (chrPos, posChr) = (IM.union (IM.fromList y') chrPos, IM.union (IM.fromList y) posChr)
  where
    [(posA, chrA), (posB, chrB)] = map (((,) <$> id <*> (posChr IM.!)) . read @Int) $ splitOn "/" xs
    y = [(posA, chrB), (posB, chrA)]
    y' = map swap y
readIns l ('p' : xs) (chrPos, posChr) = (IM.union (IM.fromList y') chrPos, IM.union (IM.fromList y) posChr)
  where
    [(posA, chrA), (posB, chrB)] = map (((,) <$> (chrPos IM.!) <*> id) . fromChar . head) $ splitOn "/" xs
    y = [(posA, chrB), (posB, chrA)]
    y' = map swap y

step :: [String] -> Pos -> Pos
step input initPos = foldl' (flip (readIns (length $ fst initPos))) initPos input

day16 :: IO ()
day16 = do
  input <- splitOn "," . init <$> readFile "input/input16.txt"
  -- input <- splitOn "," . init <$> readFile "input/test16.txt"
  putStrLn $ fromPos $ step input initPos'
  -- print $ firstCycle' $ map fromPos $ iterate (step input) initPos'
  print $ calcLargeCycleN firstCycle' n $ map fromPos $ iterate (step input) initPos'
