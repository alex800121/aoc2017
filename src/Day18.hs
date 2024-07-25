module Day18 where

import Paths_AOC2017
import Paths_AOC2017
import Control.Monad.Trans.State (State, evalState, get, modify, put, runState, state)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Vector (Vector)
import qualified Data.Vector as V
import MyLib (Parser, firstCycle', firstRepeat', signedInteger)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type M = Map Char Int

type Arg = Either Char Int

data Instruction
  = Snd Arg
  | Set Char Arg
  | Add Char Arg
  | Mul Char Arg
  | Mod Char Arg
  | Rcv Char
  | Jgz Arg Arg
  deriving (Show, Eq, Ord)

readArg :: Parser Arg
readArg = Right <$> signedInteger <|> Left <$> anySingle

readIns :: Parser Instruction
readIns =
  (string "set " >> Set <$> anySingle <* space <*> readArg)
    <|> (string "snd " >> Snd <$> readArg)
    <|> (string "add " >> Add <$> anySingle <* space <*> readArg)
    <|> (string "mul " >> Mul <$> anySingle <* space <*> readArg)
    <|> (string "mod " >> Mod <$> anySingle <* space <*> readArg)
    <|> (string "rcv " >> Rcv <$> anySingle)
    <|> (string "jgz " >> Jgz <$> readArg <* space <*> readArg)

arg :: M -> Arg -> Int
arg m = either (fromMaybe 0 . (m Map.!?)) id

step :: Vector Instruction -> State (Int, ([Int], M)) Int
step ins = do
  (pos, (sent, m)) <- get
  case ins V.! pos of
    Set r a | x <- arg m a -> put (pos + 1, (sent, Map.insert r x m)) >> step ins
    Snd a | x <- arg m a -> put (pos + 1, (x : sent, m)) >> step ins
    Add r a | x <- arg m a -> put (pos + 1, (sent, Map.alter (Just . maybe x (+ x)) r m)) >> step ins
    Mul r a | x <- arg m a -> put (pos + 1, (sent, Map.alter (Just . maybe 0 (* x)) r m)) >> step ins
    Mod r a | x <- arg m a -> put (pos + 1, (sent, Map.alter (Just . maybe 0 (`mod` x)) r m)) >> step ins
    Rcv a
      | x <- arg m (Left a) ->
          if x /= 0
            then pure $ head sent
            else put (pos + 1, (sent, m)) >> step ins
    Jgz a b
      | x <- arg m a,
        y <- arg m b ->
          if x > 0
            then put (pos + y, (sent, m)) >> step ins
            else put (pos + 1, (sent, m)) >> step ins

step' :: Vector Instruction -> [Int] -> State Program [Int]
step' ins xs = do
  P pos m <- get
  case ins V.! pos of
    Set r a | x <- arg m a -> put (P (pos + 1) (Map.insert r x m)) >> step' ins xs
    Snd a | x <- arg m a -> put (P (pos + 1) m) >> (x :) <$> step' ins xs
    Add r a | x <- arg m a -> put (P (pos + 1) (Map.alter (Just . maybe x (+ x)) r m)) >> step' ins xs
    Mul r a | x <- arg m a -> put (P (pos + 1) (Map.alter (Just . maybe 0 (* x)) r m)) >> step' ins xs
    Mod r a | x <- arg m a -> put (P (pos + 1) (Map.alter (Just . maybe 0 (`mod` x)) r m)) >> step' ins xs
    Rcv a | [] <- xs -> pure []
    Rcv a | x : xs' <- xs -> put (P (pos + 1) (Map.insert a x m)) >> step' ins xs'
    Jgz a b
      | x <- arg m a,
        y <- arg m b ->
          if x > 0
            then put (P (pos + y) m) >> step' ins xs
            else put (P (pos + 1) m) >> step' ins xs

run :: Vector Instruction -> (([Int], Program), ([Int], Program)) -> (([Int], Program), ([Int], Program))
run ins ((i0, p0), (i1, p1)) = ((o1', p0'), (o0', p1'))
  where
    (o0', p0') = runState (step' ins i0) p0
    (o1', p1') = runState (step' ins i1) p1

data Program = P
  { _pos :: Int,
    _m :: M
  }
  deriving (Show, Eq, Ord)

day18 :: IO ()
day18 = do
  input <- V.fromList . mapMaybe (parseMaybe readIns) . lines <$> (getDataDir >>= readFile . (++ "/input/input18.txt"))
  print $ evalState (step input) (0, ([], Map.empty))
  print
    . length
    . concatMap (fst . fst)
    . (\x -> let Just (a, b, _) = firstCycle' x in take (a + b) x)
    $ iterate (run input) (([], P 0 $ Map.singleton 'p' 0), ([], P 0 $ Map.singleton 'p' 1))
