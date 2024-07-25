module Day9 where

import Paths_AOC2017
import Paths_AOC2017
import MyLib (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char

processInput :: String -> String
processInput ('!' : x : xs) = processInput xs
processInput ('\n' : xs) = processInput xs
processInput (x : xs) = x : processInput xs
processInput "" = ""

data Elem = Gro Group | Gar Garbage
  deriving (Show, Eq, Ord)

data Group = Group Int [Elem]
  deriving (Show, Eq, Ord)

newtype Garbage = Garbage String
  deriving (Show, Eq, Ord)

elemParser :: Int -> Parser Elem
elemParser i = Gro <$> groupParser i <|> Gar <$> garbageParser

groupParser :: Int -> Parser Group
groupParser i = do
  char '{'
  xs <- elemParser (i + 1) `sepBy` char ','
  char '}'
  pure $ Group i xs

garbageParser :: Parser Garbage
garbageParser = char '<' >> Garbage <$> many (satisfy (/= '>')) <* char '>'

addLevel :: Elem -> Int
addLevel (Gar _) = 0
addLevel (Gro (Group n e)) = n + sum (map addLevel e)

countGarbage :: Elem -> Int
countGarbage (Gar (Garbage x)) = length x
countGarbage (Gro (Group _ e)) = sum (map countGarbage e)
day9 :: IO ()
day9 = do
  input <- (getDataDir >>= readFile . (++ "/input/input9.txt"))
  -- input <- readFile "input/test9.txt"
  let input' = parseMaybe (elemParser 1) $ processInput input
  print $ fmap addLevel input'
  print $ fmap countGarbage input'
