{-
Author  : Pieter van Wyk
Created : 2020-12-02
Updated : 2020-12-09

Implementation of quick sort
-}
module QuickSort where

-- Main Functions --
--------------------

-- sorting list with quick sort algorithm
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort small ++ [x] ++ qsort large
 where small = [a | a <- xs, a <= x]
       large = [a | a <- xs, a >  x]

-- END
