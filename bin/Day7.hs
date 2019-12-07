module Main where

import Control.Monad
import Control.Monad.Fix
import Data.List
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import System.IO.Unsafe
import Text.ParserCombinators.ReadP

runProgram :: Int -> [Int] -> VM.IOVector Int -> IO [Int]
runProgram pc input prog = do
  instr <- VM.read prog pc
  case instr `divMod` 100 of
    (mode, 1) -> handle mode (+)
    (mode, 2) -> handle mode (*)
    (mode, 3) -> case input of
      [] -> error "Out of input??"
      (x:xs) -> do
        write mode 1 x
        runProgram (pc + 2) xs prog
    (mode, 4) -> do
      output <- read mode 1
      (:) output <$> unsafeInterleaveIO (runProgram (pc + 2) input prog)
    (mode, 5) -> do
      condition <- read mode 1
      jumpAddr <- read mode 2
      if condition /= 0
      then runProgram jumpAddr input prog
      else runProgram (pc + 3) input prog
    (mode, 6) -> do
      condition <- read mode 1
      jumpAddr <- read mode 2
      if condition == 0
      then runProgram jumpAddr input prog
      else runProgram (pc + 3) input prog
    (mode, 7) -> handle mode (\x y -> fromEnum (x < y))
    (mode, 8) -> handle mode (\x y -> fromEnum (x == y))
    _ -> pure []
  where
    read :: Int -> Int -> IO Int
    read mode pos = case mode `div` 10 ^ (pos - 1) `mod` 10 of
      0 -> VM.read prog (pc + pos) >>= VM.read prog
      1 -> VM.read prog (pc + pos)
    write :: Int -> Int -> Int -> IO ()
    write mode pos val = case mode `div` 10 ^ (pos - 1) `mod` 10 of
      0 -> VM.read prog (pc + pos) >>= \addr -> VM.write prog addr val
      1 -> error "Immediate write"
    handle mode op = do
      x <- read mode 1
      y <- read mode 2
      write mode 3 (x `op` y)
      runProgram (pc + 4) input prog

evaluateSequence :: V.Vector Int -> Int -> [Int] -> IO Int
evaluateSequence prog s [] = pure s
evaluateSequence prog s (x:xs) = do
  prog' <- V.thaw prog
  [s'] <- runProgram 0 [x,s] prog'
  evaluateSequence prog s' xs

evaluateSequence2 :: V.Vector Int -> [Int] -> IO Int
evaluateSequence2 prog xs = do
  final <- mfix $ \final -> go xs (0:final)
  pure $ last final
  where
    go :: [Int] -> [Int] -> IO [Int]
    go [] ss = pure ss
    go (y:ys) ss = do
      prog' <- V.thaw prog
      ss' <- runProgram 0 (y:ss) prog'
      go ys ss'

main :: IO ()
main = do
  input <- getContents
  let [(parsedInput, "")] = readP_to_S (V.fromList <$> sepBy (readS_to_P reads) (char ',') <* char '\n') input
  results <- forM (permutations [0..4]) $ \order -> evaluateSequence parsedInput 0 order
  print . maximum $ results
  results <- forM (permutations [5..9]) $ \order -> evaluateSequence2 parsedInput order
  print . maximum $ results
