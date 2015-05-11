module Uebung where

-- data Tree = Leaf Int | Branch Tree Tree

data TA = LeafA | A TA TB deriving Show
data TB = LeafB | B TB TA deriving Show


zahlB :: TA -> Int
zahlB LeafA = 0
zahlB (A l r) = zahlB l + zahlB' r
  where
    zahlB' :: TB -> Int
    zahlB' LeafB = 0
    zahlB' (B l r) = zahlB' l + zahlB r 


insert :: Tree -> [Int] -> Tree
insert t [] = t
insert t (x:xs) = insert (ins t x) xs   -- ins x (insert t xs) 
  where ins :: Tree -> Int -> Tree
        ins Nil x = Node x Nil Nil
        ins (Node n a b) x = if x < n
                             then Node n (ins a x) b
                             else Node n a (ins b x)

isEqual :: Tree -> Tree -> Bool
isEqual Nil Nil = True
isEqual Nil _ = False
isEqual _ Nil = False
isEqual (Node n l r) (Node m u v) = if n == m then isEqual l u && isEqual r v
                                    else False

-- = n == m && isEqual l u && isEqual r v

data Tree = Node Int Tree Tree | Nil deriving (Show,Eq)

isHeap :: Tree -> Bool
isHeap Nil = True
isHeap (Node n l r) = isGreater n l && isGreater n r && isHeap l && isHeap r
  where
    isGreater :: Int -> Tree -> Bool
    isGreater _ Nil = True
    isGreater m (Node n l r) = m > n

incEntry :: [Int] -> Int -> [Int]
incEntry [] 0 = []
incEntry [] 1 = [1]
incEntry [] n = [0] ++ incEntry [] (n-1)
incEntry (x:xs) 1 = (x+1):xs
incEntry (x:xs) n = x : incEntry xs (n-1)

rsum :: [Int] -> [Int]
rsum xs = foo 0 (reverse xs)
  where foo n [] =[]
        foo n (x:xs) = (x + n) : foo (x+n) xs
