{-
Author  : Pieter van Wyk
Created : 2020-12-02
Updated : 2020-12-08

Implementation of merge sort
-}
module MergeSort where

-- Main Functions --
--------------------

-- merge two sorted lists into one sorted list
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : ( merge xs (y:ys) )
                    | otherwise = y : ( merge (x:xs) ys )

-- sort list using mergesort algorithm
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort ls = merge (mergeSort left) (mergeSort right)
  where (left, right) = let n = div (length ls) 2
                        in (take n ls, drop n ls)

-- END
