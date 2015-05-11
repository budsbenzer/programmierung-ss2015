module Vorbereitung where

data TA = NilA | A TA TB deriving Show
data TB = NilB | B TB TA deriving Show


-- Wichtig: wechselseitige Rekursion notwendig,
-- da Datentyp TA wechselseitig induktiv definiert.
zahlB :: TA -> Int
zahlB NilA    = 0
zahlB (A l r) = zahlB l + zahlB' r
  where
    zahlB' NilB = 0
    zahlB' (B l r) = 1 + zahlB' l + zahlB r

t :: TA
t = A (A NilA (B NilB NilA)) (B NilB NilA)

data Tree = Node Int Tree Tree | Nil deriving Show -- deriving (Show,Eq)

insert :: Tree -> [Int] -> Tree
insert t [] = t
insert t (x:xs) = insert (ins x t) xs
  where
    ins x Nil = Node x Nil Nil
    ins x t@(Node n l r)
      | x < n = Node n (ins x l) r
      | x > n = Node n l (ins x r)
      | otherwise = t


-- Alternativ einfach `deriving Eq` wie oben und == nutzen!
isEqual :: Tree -> Tree -> Bool
isEqual Nil Nil = True
isEqual (Node n l r) (Node m u v) = n == m && isEqual l u && isEqual r v
isEqual _ _ = False

isHeap :: Tree -> Bool
isHeap Nil = True
isHeap (Node n l r) = gt n l && gt n r && isHeap l && isHeap r
  where
    gt n Nil = True
    gt n (Node m _ _) = n >= m

rsum = sums 0 . reverse
  where sums i [] = []
        sums i (x:xs) = let i' = i + x in i' : sums i' xs

