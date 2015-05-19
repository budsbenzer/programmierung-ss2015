module Vorbereitung where

data Tree = Leaf | Branch Tree Tree deriving (Show, Eq, Ord)

height Leaf = 1
height (Branch l r) = height l `max` height r

mkBalanced 0 = []
mkBalanced 1 = [Leaf]
mkBalanced n = cartprod Branch (mkBalanced (n-1)) (mkBalanced (n-2))
               ++ cartprod Branch (mkBalanced (n-2)) (mkBalanced (n-1))
               ++ cartprod Branch (mkBalanced (n-1)) (mkBalanced (n-1))
  where
    -- Bildet das Produkt zweier Listen und wendet auf jedes Element
    -- des Produkts eine Funktion an.
    -- Bsp.: cartprod (*) [1,2] [5,7] = [1*5, 1*7, 2*5, 2*7] = [5,7,10,14]
    cartprod :: (a -> b -> c) -> [a] -> [b] -> [c]
    cartprod _ [] _ = []
    cartprod f (x:xs) ys = map (f x) ys ++ cartprod f xs ys

-- oder mit list comprehensions (https://wiki.haskell.org/List_comprehension)
mkBalanced' 0 = []
mkBalanced' 1 = [Leaf]
mkBalanced' n = [Branch l r | (hl, hr) <- [(n-2,n-1), (n-1,n-2), (n-1,n-1)]
                           , l <- mkBalanced' hl
                           , r <- mkBalanced' hr]

pow :: Int -> (a -> a) -> a -> a
-- pow 0 _ x = x
-- pow n f x = f (pow (n-1) f x)

-- pow 0 f = id
-- pow n f = f . pow (n-1) f

-- Now you are thinking higher order. ;)
pow = (foldr (.) id .) . replicate  -- (Nicht guter Stil!)


pleat :: (a -> a -> b) -> [a] -> [b]
pleat f [] = []
pleat f [_] = []
pleat f (x:y:zs) = f x y : pleat f (y:zs)

-- pleat f xs = zipWith f xs (drop 1 xs)
