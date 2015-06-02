module Vorbereitung where

import Prelude hiding (foldl)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

--   foldl (\x y -> 2*x+y) 0 [1,0,1]
-- = foldl (\x y -> 2*x+y) ((\x y -> 2*x+y) 0 1) [0,1]
-- = foldl (\x y -> 2*x+y) (2*0+1) [0,1]
-- = foldl (\x y -> 2*x+y) 1 [0,1]
-- = foldl (\x y -> 2*x+y) ((\x y -> 2*x+y) 1 0) [1]
-- = foldl (\x y -> 2*x+y) 2 [1]
-- = foldl (\x y -> 2*x+y) ((\x y -> 2*x+y) 2 1) []
-- = foldl (\x y -> 2*x+y) 5 []
-- = 5

-- Umwandlung der Binaerzahl 101 in eine Dezimalzahl.
