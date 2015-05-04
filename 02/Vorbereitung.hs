module Vorbereitung where

-- Hilfestellung: erst takeall einführen

pack [] = []
pack (x:xs) = ys : pack zs
  where
    (ys, zs) = takeall x (x:xs)
    takeall _ [] = ([], [])
    takeall x (y:ys)
      | x == y = let (us, vs) = takeall x ys 
                 in  (y:us, vs)
      | otherwise = ([], (y:ys))

encode xs = e' (pack xs)
  where e' [] = []
        e' (y:ys) = (head y, length y) : e' ys

decode [] = []
decode ((n, x) : xs) = times n x : decode xs
  where
    times 0 x = []
    times n x = x : times (n-1) x

rotate :: [Int] -> Int -> [Int]
rotate [] _ = []
rotate xss@(x:xs) n
  | n == 0 = xss
  | n < 0  = rotate xss (length xss + n)
  | otherwise = rotate (xs ++ [x]) (n - 1)


data Expr = Lit Int
          | Var Char
          | Add Expr Expr
          | Mul Expr Expr
          | Exp Expr Int
          deriving Show

diff :: Expr -> Char -> Expr
diff (Lit _) _ = Lit 0   
diff (Var x) y           
  | x == y     = Lit 1
  | otherwise  = Lit 0
diff (Add e1 e2) x = Add (diff e1 x) (diff e2 x)     
diff (Mul e1 e2) x = Add (Mul (diff e1 x) e2)        
                         (Mul e1 (diff e2 x))
diff (Exp e n) x   = (Mul (Mul (Lit n) (Exp e (n-1))) (diff e x)) 

