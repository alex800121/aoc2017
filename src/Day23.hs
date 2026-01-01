module Day23 where

import Control.Monad (when)
import Control.Monad.ST.Strict (ST, runST)
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Vector.Strict qualified as V
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed.Mutable qualified as M
import Debug.Trace
import MyLib (Parser, factors, signedInteger)
import Paths_AOC2017
import Text.Megaparsec
import Text.Megaparsec.Char

type Reg = UV.Vector Int

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

-- readIns' :: Instruction -> STReg s -> ST s ()
-- readIns' (Set c arg) s = argST s arg >>= M.write s (charToReg c) >> advance 1 s
-- readIns' (Sub c arg) s = argST s arg >>= \x -> M.modify s (subtract x) (charToReg c) >> advance 1 s
-- readIns' (Mul c arg) s = argST s arg >>= \x -> M.modify s (* x) (charToReg c) >> advance 1 s
-- readIns' (Jnz arg0 arg1) s =
--   argST s arg0 >>= \case
--     0 -> advance 1 s
--     _ -> argST s arg1 >>= flip advance s

advance :: Int -> STReg s -> ST s ()
advance i s = M.modify s (+ i) 8

initReg :: Reg
initReg = UV.fromList (replicate 10 0)

runIns :: (Instruction -> STReg s -> ST s ()) -> V.Vector Instruction -> STReg s -> ST s ()
runIns readIns v r = do
  i <- M.read r 8
  case v V.!? i of
    Nothing -> pure ()
    Just i -> do
      readIns i r
      runIns readIns v r

day23 :: IO (String, String)
day23 = do
  input <- V.fromList . mapMaybe (parseMaybe insParser) . lines <$> (getDataDir >>= readFile . (++ "/input/input23.txt"))
  let
    finalAnsa =
      show $ UV.last $ runST $ UV.thaw initReg >>= \v -> runIns readIns input v >> UV.freeze v
  let
    finalAnsb =
      show $ length $ filter ((> 2) . length . factors) [107900, 107900 + 17 .. 107900 + 17000]
  pure (finalAnsa, finalAnsb)
