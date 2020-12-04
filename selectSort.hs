{-
Author  : Pieter van Wyk
Created : 2020-12-02
Updated : 2020-12-04

Implementation of selection sort
-}

import Test.QuickCheck

-- Main Functions --
--------------------

-- swap elements at idx1 and idx2 of list ls
swap :: Int -> Int -> [a] -> [a]
swap _ _ [] = []
swap _ _ [x] = [x]
swap idx1 idx2 ls | idx1' == idx2' = ls
                  | otherwise = ls_be1 ++ (ls!!idx2':ls_bet) ++ (ls!!idx1':ls_af2)
                  where idx_ls = zip [0..] ls
                        idx1' = mod (min idx1 idx2) (length ls)
                        idx2' = mod (max idx1 idx2) (length ls)
                        ls_be1 = [ a | (n,a) <- idx_ls, n < idx1' ]
                        ls_bet = [ a | (n,a) <- idx_ls, n > idx1' && n < idx2' ]
                        ls_af2 = [ a | (n,a) <- idx_ls, n > idx2' ]

-- get index of minimum element in list, after startIdx
indexOfMinimum :: Ord a => Int -> [a] -> Int
indexOfMinimum startIdx ls = foldl f_select startIdx [(startIdx + 1)..(length ls - 1)]
                           where f_select startIdx j | ls!!j < ls!!startIdx = j
                                                     | otherwise = startIdx

-- sort list using selectSort algorithm
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = selectionSort' 0 xs
  where selectionSort' i ls | i == length ls - 1 = ls
                            | otherwise = selectionSort' (i+1) (swap i (indexOfMinimum i ls) ls)

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

-- opperating swap twice should give back the original list
prop_swap_00 :: Int -> Int -> Int -> Int -> [Int] -> Bool
prop_swap_00 idx1 idx2 e1 e2 ls = (swap' . swap') ls' == ls'
  where ls' = (e1:e2:ls)
        n = length ls'
        idx1' = mod idx1 n
        idx2' = mod idx2 n
        swap' = swap idx1' idx2'

-- let ls' be list after swapping : then ls'!!idx1 == ls!!idx2 and ls'!!idx2 == ls!!idx1 must hold
prop_swap_01 :: Int -> Int -> Int -> Int -> [Int] -> Bool
prop_swap_01 idx1 idx2 e1 e2 ls = ls_be!!idx1' == ls_af!!idx2' && ls_be!!idx2' == ls_af!!idx1'
  where ls_be = (e1:e2:ls)
        n = length ls_be
        idx1' = mod idx1 n
        idx2' = mod idx2 n
        ls_af = swap idx1' idx2' ls_be

-- property of sorted list : index of minumum element after startIdx is startIdx
prop_indexOfMinimum_00 :: Int -> Int -> [Int] -> Bool
prop_indexOfMinimum_00 e1 startIdx ls = indexOfMinimum startIdx' ls' == startIdx'
  where ls' = selectionSort (e1:ls)
        startIdx' = mod startIdx (length ls')

-- check if list is sorted after select sort
prop_selectsort_00 :: [Int] -> Bool
prop_selectsort_00 xs = prop_sort selectionSort xs

main = do
  quickCheck (withMaxSuccess 10000 prop_swap_00)
  quickCheck (withMaxSuccess 10000 prop_swap_01)
  quickCheck (withMaxSuccess 10000 prop_indexOfMinimum_00)
  quickCheck (withMaxSuccess 10000 prop_selectsort_00)
