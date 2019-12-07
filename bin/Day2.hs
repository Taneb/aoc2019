module Main where

import Control.Monad
import qualified Data.Vector as V
import Text.ParserCombinators.ReadP

runProgram :: V.Vector Int -> Int -> V.Vector Int
runProgram prog pc =
  let instr = prog V.! pc
  in case instr of
    1 -> handle (+)
    2 -> handle (*)
    _ -> prog
  where
    handle op =
      let x = prog V.! (prog V.! (pc + 1))
          y = prog V.! (prog V.! (pc + 2))
      in runProgram (prog V.// [(prog V.! (pc + 3), x `op` y)]) (pc + 4)

main :: IO ()
main = do
  input <- getContents
  let [(parsedInput, "")] = readP_to_S (V.fromList <$> sepBy (readS_to_P reads) (char ',') <* char '\n') input
  print $ runProgram (parsedInput V.// [(1,12), (2,2)]) 0 V.! 0
  forM_ [0..99] $ \noun ->
    forM_ [0..99] $ \verb ->
      when (runProgram (parsedInput V.// [(1,noun),(2,verb)]) 0 V.! 0 == 19690720) $ print (100 * noun + verb)
