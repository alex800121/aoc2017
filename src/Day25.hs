module Day25 (day25) where

import Control.Monad.ST.Strict (runST)
import Data.Bits (Bits (..))
import Data.Char (isAlpha, ord)
import Data.Maybe (fromJust)
import Data.Vector.Generic.Mutable qualified as MGV
import Data.Vector.Storable.Mutable qualified as MSV
import Data.Vector.Unboxed qualified as UV
import Data.Word (Word64)
import MyLib (Parser, signedInteger)
import Paths_AOC2017
import Text.Megaparsec
import Text.Megaparsec.Char

type SubRule = (Bool, Bool, Int)
type Rule = (SubRule, SubRule)

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

rulesParser :: Parser Rule
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
  pure ((writeOne, moveRight, ord toChar - ord 'A'), (writeOne1, moveRight1, ord toChar1 - ord 'A'))

{-
Begin in state A.
Perform a diagnostic checksum after 12523873 steps.
-}

run rules !step !r len !posIn !posOut !cur v
  | step <= 0 = MSV.write v posOut cur >> MSV.foldl' (\acc x -> popCount x + acc) 0 v
  -- | posIn < 0 && posOut - 1 < 0 = do
  --     MSV.write v posOut cur
  --     run rules step r (len + 1) (cell - 1) 0 0 =<< MGV.growFront v 1
  -- | posIn >= cell && posOut + 1 >= len = do
  --     MSV.write v posOut cur
  --     run rules step r (len + 1) 0 (posOut + 1) 0 =<< MGV.grow v 1
  | posIn < 0 = MSV.write v posOut cur >> MSV.read v (posOut - 1) >>= \cur' -> run rules step r len (cell - 1) (posOut - 1) cur' v
  | posIn >= cell = MSV.write v posOut cur >> MSV.read v (posOut + 1) >>= \cur' -> run rules step r len 0 (posOut + 1) cur' v
  | otherwise = run rules (step - 1) r' len posIn' posOut cur' v
  where
    ((w0, r0, n0), (w1, r1, n1)) = rules UV.! r
    b = testBit cur posIn
    cur'
      | b && w1 = setBit cur posIn
      | b = clearBit cur posIn
      | w0 = setBit cur posIn
      | otherwise = clearBit cur posIn
    posIn'
      | b && r1 = posIn + 1
      | b = posIn - 1
      | r0 = posIn + 1
      | otherwise = posIn - 1
    r' = if b then n1 else n0

cell = 64
len = 400

inputParser :: Parser (Int, UV.Vector Rule, Int)
inputParser = do
  s <- string "Begin in state " >> anySingle <* char '.' <* newline
  i <- string "Perform a diagnostic checksum after " >> signedInteger <* string " steps." <* newline <* newline
  r <- rulesParser `sepBy` newline
  pure (i, UV.fromList r, ord s - ord 'A')

day25 :: IO (String, String)
day25 = do
  (target, rules, start) <- fromJust . parseMaybe inputParser <$> (getDataDir >>= readFile . (++ "/input/input25.txt"))
  -- (target, rules, start) <- fromJust . parseMaybe inputParser <$> (getDataDir >>= readFile . (++ "/input/test25.txt"))
  let finalAnsa = show $ runST $ run rules target start len (cell `div` 2) (len `div` 2) 0 =<< MSV.replicate len (0 :: Word64)
      finalAnsb = "Merry Christmas!!!"
  pure (finalAnsa, finalAnsb)
