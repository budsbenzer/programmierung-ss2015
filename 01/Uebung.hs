module Uebung where

sum' :: Int -> Int
sum' 0 = 0
sum' n = n + sum' (n - 1)

square :: [Int] -> [Int]
square [] = []
square (x:xs) = (x * x) : square xs

at :: [Int] -> Int -> Maybe Int
at [] n = Nothing
at (x:xs) 0 = x
at (x:xs) n = at xs (n-1)

dup' [] _ = []
dup' _ [] = []
dup' (x:xs) ys = isThere x ys ++ dup' xs ys

dup [] _ = []
dup _ [] = []
dup (x:xs) ys = if elem' x ys then x : dup xs ys else dup xs ys 

elem' :: Int -> [Int] -> Bool
elem' _ [] = False
elem' x (y:ys)
  -- | x == y = True
  -- | otherwise = elem' x ys
  = if x == y then True else elem' x ys

isThere :: Int -> [Int] -> [Int]
isThere x [] = []
isThere x (y:ys)
  | x == y  = [x]
  | otherwise  = isThere x ys


f [] = []
f (x:xs)
  | x <= 0 = f xs
  | otherwise = f xs ++ [x]

append :: [Int] -> [Int] -> [Int]
append [] ys = ys
append (x:xs) ys = x: append xs ys

s :: [Int]
s = s' 1
  where s' n = n : s' (n+1)
