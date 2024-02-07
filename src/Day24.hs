module Day24 where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (delete, sort)
import Data.List.Split (splitOn)
import Debug.Trace (traceShowM)
import MyLib (pickAnySplit)
import Data.Function (on)
import Data.Foldable (maximumBy)

type Port = Int

type Component = [Port]

type Bridge = [Component]

buildBridge :: [Component] -> Port -> [Bridge]
buildBridge c p =
  ( do
      (p0, ps') <- pickAnySplit c
      guard $ p `elem` p0
      let p1 = head $ delete p p0
      (p0 :) <$> buildBridge ps' p1
  )
    <|> pure []

day24 :: IO ()
day24 = do
  input <- sort . map (sort . map (read @Int) . splitOn "/") . lines <$> readFile "input/input24.txt"
  let b = buildBridge input 0
  print $ maximum $ map (sum . map sum) b
  print $ sum . map sum $ maximumBy (\x y -> (compare `on` length) x y <> (compare `on` sum . map sum) x y) b

-- print input
