module Main where

import qualified Data.Map.Strict as M

collapseTree :: M.Map String [String] -> (Int, Int)
collapseTree m = go "COM"
  where
  go a =
    let
      nexts = M.findWithDefault [] a m 
      (ndecs, ncons) = unzip (map go nexts)
    in (1 + sum ndecs, sum ndecs + sum ncons)

buildTree :: [(String,String)] -> M.Map String [String]
buildTree [] = M.empty
buildTree ((a,b):r) = M.insertWith (++) a [b] $ buildTree r

{-
I want to convert to a symmetric graph and then do a breadth-first search
-}

symmetrify :: M.Map String [String] -> M.Map String [String]
symmetrify = M.foldrWithKey f M.empty
  where
    f :: String -> [String] -> M.Map String [String] -> M.Map String [String]
    f x xs = M.insertWith (++) x xs . flip (foldr (\a -> M.insertWith (++) a [x])) xs

bfs :: M.Map String [String] -> Int
bfs m0 = let Left r = loop m0 0 ["YOU"] in r
  where
    next :: M.Map String [String] -> [String] -> [String]
    next m = concatMap scan
      where
        scan :: String -> [String]
        scan s = M.findWithDefault [] s m

    go :: M.Map String [String] -> Int -> [String] -> Either Int (M.Map String [String], Int, [String])
    go m n ss = case next m ss of
      [] -> error "failed"
      xs | "SAN" `elem` xs -> Left n
         | otherwise       -> Right (foldr M.delete m ss, n + 1, xs)

    loop :: M.Map String [String] -> Int -> [String] -> Either Int a
    loop m n ss = go m n ss >>= uncurry3 loop

    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f (a,b,c) = f a b c

main :: IO ()
main = do
  c <- buildTree . map (\x -> (take 3 x, drop 4 x)) . lines <$> getContents
  print . snd . collapseTree $ c
  print . subtract 1 . bfs . symmetrify $ c
