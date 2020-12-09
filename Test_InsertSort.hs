{-
Author  : Pieter van Wyk
Created : 2020-12-08
Updated : 2020-12-09

Unit tests for insert sort algorithm
-}
import InsertSort
import TestSortModule
import Test.QuickCheck

-- Unit Tests --
----------------

-- check if list remains sorted after insert
prop_insert_00 :: Int -> [Int] -> Bool
prop_insert_00 x xs = isSorted $ insert x (insertSort xs)

-- check if list is sorted after insertSort
prop_insert_01 :: [Int] -> Bool
prop_insert_01 = prop_sort insertSort

main = do
  quickCheck (withMaxSuccess 10000 prop_insert_00)
  quickCheck (withMaxSuccess 10000 prop_insert_01)
