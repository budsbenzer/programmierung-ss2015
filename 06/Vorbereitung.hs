module Vorbereitung where

-- Uebung 1

data Tree a = Node a (Tree a) (Tree a) | Leaf a deriving Show

class Zippable f where
  genericZip     :: f a -> f b -> f (a, b)
  genericZipWith :: (a -> b -> c) -> f a -> f b -> f c

  -- Standardimplementierung von genericZip durch genericZipWith
  genericZip = genericZipWith (,)
  -- Wäre f zusätzlich Instanz von Funktor, dann ginge es auch anders rum:
  --   genericZipWith f u v = fmap (uncurry f) (genericZip u v)


instance Zippable Tree where
  genericZip (Leaf a) (Leaf b) = Leaf (a, b)
  genericZip (Leaf a) (Node b _ _) = Leaf (a, b)
  genericZip (Node a s t) (Node b u v) = Node (a, b) (genericZip s u) (genericZip t v)

  genericZipWith f (Leaf a) (Leaf b) = Leaf (f a b)
  genericZipWith f (Leaf a) (Node b _ _) = Leaf (f a b)
  genericZipWith f (Node a s t) (Node b u v)
    = Node (f a b) (genericZipWith f s u) (genericZipWith f t v)


data RoseTree a = RNode a [RoseTree a] deriving Show

instance Zippable RoseTree where
  genericZipWith f (RNode a ls) (RNode b rs)
    = RNode (f a b) (zipWith (genericZipWith f) ls rs)
