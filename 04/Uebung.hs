module Uebung where

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show

paths :: Tree a -> Tree Int
paths (Leaf _) = Leaf 1
paths (Node x l r) = Node 1 (inc (paths l)) (inc (paths r))
  where
    inc :: Tree Int -> Tree Int
    inc (Leaf x) = Leaf (x + 1)
    inc (Node x l r) = Node (x + 1) (inc l) (inc r)

nodes :: Tree a -> Tree [a]
nodes (Leaf x) = Leaf [x]
nodes (Node x l r) = Node [x, root l, root r] (nodes l) (nodes r)
  where
    root :: Tree a -> a
    root (Leaf x) = x
    root (Node x _ _) = x


isGreaterFive :: Int -> Bool
isGreaterFive x
  | x > 5 = True
  | otherwise = False

f :: [Int] -> [Int]
-- f xs = reverse (filter (>5) xs)
f = reverse . filter (>5)

pow :: Int -> (a -> a) -> (a -> a)
pow 0 f = id
-- pow 1 f = f  (ueberfluessig)
pow n f = f . pow (n-1) f

-- pow 0 f x = x
-- pow n f x = f (pow (n-1) f x)

pleat :: (a -> a -> b) -> [a] -> [b]
pleat _ [] = []
pleat _ [_] = []
pleat f (x:y:xs) = (f x y) : pleat f (y:xs)

fib = 1 : 1 : pleat (+) fib

inorder :: Tree a -> [a]
inorder (Leaf x) = [x]
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node a l r) = Node (f a) (mapTree f l) (mapTree f r)
