module Vorbereitung where

square [] = []
square (x:xs) = (x*x) : square xs

maxLength :: [[Int]] -> Int
maxLength [] = 0
-- maxLength (l:ls) = max (length l) (maxLength ls)
---------
-- maxLength (l:ls)
--   | length l > maxLength ls = length l
--   | otherwise               = maxLength ls
---------
maxLength (l:ls)
  | ll > lls = ll
  | otherwise = lls
  where ll  = length l
        lls = maxLength ls
                              



s = gen 1
  where gen i = i : gen (i + 1)
