{-
Author  : Pieter van Wyk
Created : 2020-12-02
Updated : 2020-12-02

Implementation of quick sort
-}

import Test.QuickCheck

-- Main Functions --
--------------------

-- sorting list with quick sort algorithm
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort small ++ [x] ++ qsort large
 where small = [a | a <- xs, a <= x]
       large = [a | a <- xs, a >  x]


-- Unit Tests --
----------------

-- test helper function 1
-- check if list is sorted
isSorted :: Ord a => [a] -> Bool
isSorted xs = case xs of
               [] -> True
               [x] -> True
               (x1:x2:xs') -> if x1 > x2
                              then False
                              else isSorted (x2:xs')

-- test helper function 2
-- function that takes a list and sorting algorithm
-- and checks if the list is sorted by the algorithm
prop_sort :: ([Int] -> [Int]) -> [Int] -> Bool
prop_sort sort_f = isSorted . sort_f

main = do
  quickCheck (withMaxSuccess 10000 (prop_sort qsort))
