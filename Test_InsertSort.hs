{-
Author  : Pieter van Wyk
Created : 2020-12-08
Updated : 2020-12-08

Unit tests for insert sort algorithm
-}
import InsertSort
import Test.QuickCheck

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

-- check if list remains sorted after insert
prop_insert_00 :: Int -> [Int] -> Bool
prop_insert_00 x xs = isSorted $ insert x (insertSort xs)

-- check if list is sorted after insertSort
prop_insert_01 :: [Int] -> Bool
prop_insert_01 = prop_sort insertSort

main = do
  quickCheck (withMaxSuccess 10000 prop_insert_00)
  quickCheck (withMaxSuccess 10000 prop_insert_01)
