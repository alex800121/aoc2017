module Day23 where

import Control.Monad.ST.Lazy (ST, runST)
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import Debug.Trace
import MyLib (Parser, signedInteger, factors)
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad (when)

type Reg = V.Vector Int

type STReg s = M.STVector s Int

type Arg = Either Char Int

data Instruction
  = Set Char Arg
  | Sub Char Arg
  | Mul Char Arg
  | Jnz Arg Arg
  deriving (Show, Eq, Ord)

readArg :: Parser Arg
readArg = Right <$> signedInteger <|> Left <$> anySingle

insParser :: Parser Instruction
insParser =
  (string "set " >> Set <$> anySingle <* space <*> readArg)
    <|> (string "sub " >> Sub <$> anySingle <* space <*> readArg)
    <|> (string "mul " >> Mul <$> anySingle <* space <*> readArg)
    <|> (string "jnz " >> Jnz <$> readArg <* space <*> readArg)

argST :: STReg s -> Arg -> ST s Int
argST m arg = case arg of
  -- argST m arg = V.freeze m >>= traceShowM >> case arg of
  Right i -> pure i
  Left c -> M.read m (charToReg c)

charToReg = subtract (ord 'a') . ord

readIns :: Instruction -> STReg s -> ST s ()
readIns (Set c arg) s = argST s arg >>= M.write s (charToReg c) >> advance 1 s
readIns (Sub c arg) s = argST s arg >>= \x -> M.modify s (subtract x) (charToReg c) >> advance 1 s
readIns (Mul c arg) s = argST s arg >>= \x -> M.modify s (* x) (charToReg c) >> M.modify s (+ 1) 9 >> advance 1 s
readIns (Jnz arg0 arg1) s =
  argST s arg0 >>= \case
    0 -> advance 1 s
    _ -> argST s arg1 >>= flip advance s

readIns' :: Instruction -> STReg s -> ST s ()
readIns' (Set c arg) s = argST s arg >>= M.write s (charToReg c) >> advance 1 s
readIns' (Sub c arg) s = argST s arg >>= \x -> M.modify s (subtract x) (charToReg c) >> advance 1 s
readIns' (Mul c arg) s = argST s arg >>= \x -> M.modify s (* x) (charToReg c) >> advance 1 s
readIns' (Jnz arg0 arg1) s =
  argST s arg0 >>= \case
    0 -> advance 1 s
    _ -> argST s arg1 >>= flip advance s
advance :: Int -> STReg s -> ST s ()
advance i s = M.modify s (+ i) 8

initReg :: Reg
initReg = V.fromList (replicate 10 0)

runIns :: (Instruction -> STReg s -> ST s ()) -> V.Vector Instruction -> STReg s -> ST s ()
runIns readIns v r = do
  -- v0 <- V.freeze r
  i <- M.read r 8
  case v V.!? i of
    Nothing -> pure ()
    Just i -> do
      readIns i r
      -- v1 <- V.freeze r
      -- when (v0 V.! 6 == 0) $ traceShowM (v0, v1)
      runIns readIns v r


a = V.fromList [1,107900,124900,2,53950,1,0,0,15,0]
day23 :: IO ()
day23 = do
  input <- V.fromList . mapMaybe (parseMaybe insParser) . lines <$> readFile "input/input23.txt"
  -- print input
  print $ runST $ V.thaw initReg >>= \v -> runIns readIns input v >> V.freeze v
  print $ length $ filter ((> 2) . length . factors)  [107900, 107900+17 .. 107900+ 17000]
