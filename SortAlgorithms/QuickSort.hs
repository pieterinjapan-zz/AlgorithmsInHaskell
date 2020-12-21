{-
Author  : Pieter van Wyk
Created : 2020-12-02
Updated : 2020-12-21

Implementation of quick sort algorithm
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

-- quick sort using foldr, derived from universal property
qsortFold :: Ord a => [a] -> [a]
qsortFold [] = []
qsortFold (x:xs) = qsortFold small ++ [x] ++ qsortFold large
 where split_ls f x = foldr (\y v -> if f y x then y:v else v) []
       small = split_ls (<=) x xs
       large = split_ls (>)  x xs

-- END
