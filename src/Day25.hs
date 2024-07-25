module Day25 where

import Paths_AOC2017
import Paths_AOC2017
import Control.Monad.ST.Strict (ST, runST)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.STRef.Strict (modifySTRef', newSTRef, readSTRef)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Void (Void)
import GHC.STRef (STRef (STRef))
import MyLib (Parser, signedInteger)
import Text.Megaparsec
import Text.Megaparsec.Char

type Rules = Map Char (Rule, Rule)

data Tape a = Tape
  { _left :: ![a],
    _focus :: !a,
    _right :: ![a]
  }
  deriving (Show, Eq, Ord)

write :: Tape a -> a -> Tape a
write (Tape l _ r) c = Tape l c r

moveLeft :: Tape a -> Maybe (Tape a)
moveLeft (Tape [] c r) = Nothing
moveLeft (Tape (x : xs) c r) = Just (Tape xs x (c : r))

moveRight :: Tape a -> Maybe (Tape a)
moveRight (Tape l c []) = Nothing
moveRight (Tape l c (x : xs)) = Just (Tape (c : l) x xs)

moveLeft' = moveLeftWithDefault False

moveRight' = moveRightWithDefault False

moveLeftWithDefault :: a -> Tape a -> Tape a
moveLeftWithDefault a t = fromMaybe (Tape [] a (_focus t : _right t)) (moveLeft t)

moveRightWithDefault :: a -> Tape a -> Tape a
moveRightWithDefault a t = fromMaybe (Tape (_focus t : _left t) a []) (moveRight t)

data Rule = R
  { _writeOne :: !Bool,
    _moveRight :: !Bool,
    _toChar :: !Char
  }
  deriving (Show, Eq, Ord)

data GameState = G
  { _state :: !Char,
    _tape :: !(Tape Bool)
  }
  deriving (Show, Eq, Ord)

{-
In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state E.
-}

rulesParser :: Parser Rules
rulesParser = do
  k <- string "In state " >> anySingle <* char ':' <* newline
  string "  If the current value is 0:" >> newline
  writeOne <- (== 1) <$> (string "    - Write the value " >> signedInteger <* char '.' <* newline)
  moveRight <- (== "right") <$> (string "    - Move one slot to the " >> many (satisfy isAlpha) <* char '.' <* newline)
  toChar <- string "    - Continue with state " >> anySingle <* char '.' <* newline
  string "  If the current value is 1:" >> newline
  writeOne1 <- (== 1) <$> (string "    - Write the value " >> signedInteger <* char '.' <* newline)
  moveRight1 <- (== "right") <$> (string "    - Move one slot to the " >> many (satisfy isAlpha) <* char '.' <* newline)
  toChar1 <- string "    - Continue with state " >> anySingle <* char '.' <* newline
  pure $ Map.singleton k (R writeOne moveRight toChar, R writeOne1 moveRight1 toChar1)

{-
Begin in state A.
Perform a diagnostic checksum after 12523873 steps.
-}

inputParser :: Parser (Int, Rules, GameState)
inputParser = do
  s <- string "Begin in state " >> anySingle <* char '.' <* newline
  i <- string "Perform a diagnostic checksum after " >> signedInteger <* string " steps." <* newline <* newline
  r <- Map.unions <$> (rulesParser `sepBy` newline)
  pure (i, r, G s (Tape [] False []))

step :: Rules -> GameState -> GameState
step rules g@(G s t) = applyRule (if x then r1 else r0) g
  where
    (r0, r1) = rules Map.! s
    x = _focus t

applyRule :: Rule -> GameState -> GameState
applyRule (R o r n) (G c t) = G n ((if r then moveRight' else moveLeft') (write t o))

calc :: Tape Bool -> Int
calc (Tape l c r) = length (filter id l) + length (filter id (c : r))

day25a :: Rules -> Int -> GameState -> GameState
day25a rules n g = runST $ do
  gst <- newSTRef g
  f n gst
  readSTRef gst
  where
    f :: Int -> STRef s GameState -> ST s ()
    f n x
      | n <= 0 = pure ()
      | otherwise = modifySTRef' x (step rules) >> f (n - 1) x

day25 :: IO ()
day25 = do
  (target, rules, initState) <- fromJust . parseMaybe inputParser <$> (getDataDir >>= readFile . (++ "/input/input25.txt"))
  print $ calc $ _tape $ day25a rules target initState
