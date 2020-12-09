{-
Author  : Pieter van Wyk
Created : 2020-12-02
Updated : 2020-12-09

Unit tests for selection sort algorithm
-}
import SelectSort
import TestSortModule
import Test.QuickCheck

-- Unit Tests --
----------------

-- opperating swap twice should give back the original list
prop_select_00 :: Int -> Int -> Int -> Int -> [Int] -> Bool
prop_select_00 idx1 idx2 e1 e2 ls = (swap' . swap') ls' == ls'
  where ls' = (e1:e2:ls)
        n = length ls'
        idx1' = mod idx1 n
        idx2' = mod idx2 n
        swap' = swap idx1' idx2'

-- let ls' be list after swapping : then ls'!!idx1 == ls!!idx2 and ls'!!idx2 == ls!!idx1 must hold
prop_select_01 :: Int -> Int -> Int -> Int -> [Int] -> Bool
prop_select_01 idx1 idx2 e1 e2 ls = ls_be!!idx1' == ls_af!!idx2' && ls_be!!idx2' == ls_af!!idx1'
  where ls_be = (e1:e2:ls)
        n = length ls_be
        idx1' = mod idx1 n
        idx2' = mod idx2 n
        ls_af = swap idx1' idx2' ls_be

-- property of sorted list : index of minumum element after startIdx is startIdx
prop_select_02 :: Int -> Int -> [Int] -> Bool
prop_select_02 e1 startIdx ls = indexOfMinimum startIdx' ls' == startIdx'
  where ls' = selectionSort (e1:ls)
        startIdx' = mod startIdx (length ls')

-- check if list is sorted after select sort
prop_select_03 :: [Int] -> Bool
prop_select_03 xs = prop_sort selectionSort xs

main = do
  quickCheck (withMaxSuccess 10000 prop_select_00)
  quickCheck (withMaxSuccess 10000 prop_select_01)
  quickCheck (withMaxSuccess 10000 prop_select_02)
  quickCheck (withMaxSuccess 10000 prop_select_03)
