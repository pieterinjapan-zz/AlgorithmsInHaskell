{-
Author  : Pieter van Wyk
Created : 2020-12-02
Updated : 2020-12-02

Implementation of merge sort
-}

import Test.QuickCheck

-- Main Functions --
--------------------

-- merge two sorted lists into one sorted list :
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y     = x : ( merge xs (y:ys) )
                    | otherwise = y : ( merge (x:xs) ys )

-- sort list using mergesort algorithm :
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort ls = merge (mergeSort left) (mergeSort right)
  where (left, right) = let n = div (length ls) 2
                        in (take n ls, drop n ls)

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

-- check if merging two sorted lists gives a sorted list
prop_msort_00 :: [Int] -> [Int] -> Bool
prop_msort_00 xs ys = isSorted (merge xs' ys')
  where xs' = mergeSort xs
        ys' = mergeSort ys

-- check if sorted list is sorted after insert
prop_msort_01 :: [Int] -> Bool
prop_msort_01 = prop_sort mergeSort

main = do
  quickCheck (withMaxSuccess 10000 prop_msort_00)
  quickCheck (withMaxSuccess 10000 prop_msort_01)
