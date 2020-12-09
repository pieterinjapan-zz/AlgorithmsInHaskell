{-
Author  : Pieter van Wyk
Created : 2020-12-08
Updated : 2020-12-09

Unit tests for merge sort algorithm
-}
import MergeSort
import TestSortModule
import Test.QuickCheck

-- Unit Tests --
----------------

-- check if merging two sorted lists gives a sorted list
prop_msort_00 :: [Int] -> [Int] -> Bool
prop_msort_00 xs ys = isSorted (merge xs' ys')
  where xs' = mergeSort xs
        ys' = mergeSort ys

-- check if sorted list is sorted after merge sort
prop_msort_01 :: [Int] -> Bool
prop_msort_01 = prop_sort mergeSort

main = do
  quickCheck (withMaxSuccess 10000 prop_msort_00)
  quickCheck (withMaxSuccess 10000 prop_msort_01)
