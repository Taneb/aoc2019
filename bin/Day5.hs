module Main where

import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
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
      (:) output <$> runProgram (pc + 2) input prog
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

main :: IO ()
main = do
  input <- getContents
  let [(parsedInput, "")] = readP_to_S (V.fromList <$> sepBy (readS_to_P reads) (char ',') <* char '\n') input
  [output] <- dropWhile (== 0) <$> (runProgram 0 [1] =<< V.thaw parsedInput)
  print output
  [output] <- runProgram 0 [5] =<< V.thaw parsedInput
  print output
