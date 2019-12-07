module Main where

import Data.Maybe
import qualified Data.IntMap as M
import Text.ParserCombinators.ReadP

type Interval = ((Int,Int),(Int,Bool))
type Slice = [Interval]

slinsert :: Interval -> Slice -> Slice
slinsert i [] = [i]
slinsert ((lo,hi),s) (((lo',hi'),s'):r)
  | lo > hi' = ((lo',hi'),s') : slinsert ((lo,hi),s) r
  | hi < lo' = ((lo,hi),s) : ((lo', hi'),s') : r
  | otherwise = error "I don't think this happens" slinsert (min lo lo', max hi hi') r

sliceContains :: Int -> Slice -> Maybe Int
sliceContains i [] = Nothing
sliceContains i (((lo, hi),s):r)
  | lo <= i && i <= hi = Just $ if snd s then lo - i + fst s else hi - i + fst s
  | otherwise          = sliceContains i r

update :: Int -> Interval -> M.IntMap Slice -> M.IntMap Slice
update k i m = M.alter f k m
  where
    f Nothing =  Just [i]
    f (Just x) = Just $ slinsert i x

data Dir = U | R | D | L deriving (Eq, Read, Show)

encodePath :: [(Dir, Int)] -> (M.IntMap Slice, M.IntMap Slice)
encodePath = go 0 0 0 M.empty M.empty
  where
    go x y d hs vs [] = (hs, vs)
    go x y d hs vs ((dir, dist):r) = case dir of
      U -> go x (y + dist) (d + dist) hs (update x ((y, y + dist),(d,True)) vs) r
      R -> go (x + dist) y (d + dist) (update y ((x, x + dist),(d,True)) hs) vs r
      D -> go x (y - dist) (d + dist) hs (update x ((y - dist, y),(d,False)) vs) r
      L -> go (x - dist) y (d + dist) (update y ((x - dist, x),(d,False)) hs) vs r

checkPath :: M.IntMap Slice -> M.IntMap Slice -> [(Dir, Int)] -> [((Int, Int), Int)]
checkPath hs vs = go 0 0 0
  where
    go x y d [] = []
    go x y d ((dir, dist):r) = case dir of
      U -> mapMaybe (\(x',y',d') -> (,) (x',y') . (+) d' <$> sliceContains x' (M.findWithDefault [] y' hs)) [(x,y+i,d+i) | i <- [0..dist]] ++ go x (y + dist) (d + dist) r
      R -> mapMaybe (\(x',y',d') -> (,) (x',y') . (+) d' <$> sliceContains y' (M.findWithDefault [] x' vs)) [(x+i,y,d+i) | i <- [0..dist]] ++ go (x + dist) y (d + dist) r
      D -> mapMaybe (\(x',y',d') -> (,) (x',y') . (+) d' <$> sliceContains x' (M.findWithDefault [] y' hs)) [(x,y-dist+i,d+i) | i <- [0..dist]] ++ go x (y - dist) (d + dist) r
      L -> mapMaybe (\(x',y',d') -> (,) (x',y') . (+) d' <$> sliceContains y' (M.findWithDefault [] x' vs)) [(x-dist+i,y,d+i) | i <- [0..dist]] ++ go (x - dist) y (d + dist) r

parsePath :: ReadS [(Dir,Int)]
parsePath = readP_to_S parser
  where
    parser = sepBy p1 (char ',') <* eof
    p1 = (,) <$> pdir <*> readS_to_P reads
    pdir = choice
      [ U <$ char 'U'
      , R <$ char 'R'
      , D <$ char 'D'
      , L <$ char 'L'
      ]

manhattanDistance :: Int -> Int -> Int
manhattanDistance x y = abs x + abs y

main :: IO ()
main = do
  [p1,p2] <- lines <$> getContents
  let (hs,vs) = let [(p,_)] = parsePath p1 in encodePath p
  let cs = let [(p,_)] = parsePath p2 in checkPath hs vs p
  print . minimum . map (uncurry manhattanDistance) . filter (/= (0,0)) $ fmap fst cs
  print . minimum . filter (/= 0) $ fmap snd cs
