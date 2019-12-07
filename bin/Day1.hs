module Main where

import Control.Arrow
import Data.Bifoldable

calculateFuelFromMass :: Int -> Int
calculateFuelFromMass mass = mass `div` 3 - 2

calculateFuelFromMass2 :: Int -> Int
calculateFuelFromMass2 mass = case calculateFuelFromMass mass of
  fuel | fuel <= 0 -> 0
       | otherwise -> fuel + calculateFuelFromMass2 fuel

main :: IO ()
main = getContents >>= bitraverse_ print print . (sum *** sum) . unzip . map (calculateFuelFromMass &&& calculateFuelFromMass2 <<< read) . lines
