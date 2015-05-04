module Uebung where

-- Aufgabe 1
-- (a)

compare' :: [Int] -> [Int] -> Bool
compare' [] [] = True
compare' [] _  = False
compare' _  [] = False
compare' (x:xs) (y:ys) = (x == y) && compare' xs ys


-- Aufgabe 3
-- (1)

data Tree = Leaf Int | Branch Tree Tree

countLeaves :: Tree -> Int
countLeaves (Leaf _) = 1
countLeaves (Branch t1 t2) = countLeaves t1 + countLeaves t2

yield :: Tree -> [Int]
yield (Leaf i) = [i]
yield (Branch t1 t2) = yield t1 ++ yield t2





-- Aufgabe 4 ----------

data Expr = Lit Int
          | Var Char
          | Add Expr Expr
          | Mul Expr Expr
          | Exp Expr Int
          deriving Show

type Assignment = Char -> Int

a :: Assignment
a 'x' = 2
a 'y' = 3
a _   = 0

e :: Expr
e = Add (Mul (Lit 5) (Exp (Var 'x') 2))
    (Mul (Var 'x') (Var 'y'))

eval :: Expr -> Assignment -> Int
eval (Lit i) a = i
eval (Var x) a = a x
eval (Add e1 e2) a = eval e1 a + eval e2 a
eval (Mul e1 e2) a = eval e1 a * eval e2 a
eval (Exp e i) a = eval e a ^ i

display :: Expr -> String
display (Lit i) = show i
display (Var x) = x : "" -- [x]
display (Add e1 e2) = "(" ++ display e1 ++ " + " ++ display e2 ++ ")"
display (Mul e1 e2) = display e1 ++ " * " ++ display e2
display (Exp e i) = "(" ++ display e ++ ")^" ++ show i

pack :: [Char] -> [[Char]] -- String -> [String]
pack [] = []
pack (x:xs) = let (us,vs) = split x (x:xs)
              in us : pack vs

split :: Char -> [Char] -> ([Char], [Char])
split x [] = ([], [])
split x (y:ys)
  | x == y = let (us,vs) = split x ys
             in  (x : us, vs)
  | otherwise = ([], y:ys)
