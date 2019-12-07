module Main where

c1 :: String -> Bool
c1 p = any (uncurry (==)) $ zip p (tail p)

c2 :: String -> Bool
c2 (c:p) = go c p
  where
    -- c could be the start of a solution
    go c (d:[]) = c == d
    go c (d:p)
      | c == d = stop d p
      | c /= d = go d p
    go c [] = False

    -- c could be the end of a solution
    stop c [] = True
    stop c (d:p)
      | c == d = hold d p
      | c /= d = True

    -- we need to clear c
    hold c [] = False
    hold c (d:p)
      | c == d = hold d p
      | c /= d = go d p

next :: String -> String
next ('9':p) = case next p of
  [] -> error "no"
  (d:q) -> d:d:q
next (x:p) = succ x:p

main :: IO ()
main = do
  print $ length $ filter c1 $ takeWhile (/= "999996") $ iterate next "999741"
  print $ length $ filter c2 $ takeWhile (/= "999996") $ iterate next "999741"
