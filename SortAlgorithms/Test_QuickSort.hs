{-
Author  : Pieter van Wyk
Created : 2020-12-08
Updated : 2020-12-09

Property tests for quick sort algorithm
-}
import QuickSort
import TestSortModule
import Test.QuickCheck

-- Property Tests --
--------------------

-- check if list is sorted after quick sort
prop_quick_00 = prop_sort qsort

main = do
  quickCheck (withMaxSuccess 10000 prop_quick_00)
