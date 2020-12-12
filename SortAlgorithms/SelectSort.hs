{-
Author  : Pieter van Wyk
Created : 2020-12-02
Updated : 2020-12-09

Implementation of selection sort algorithm
-}
module SelectSort where

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

-- sort list using selection sort algorithm
selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort xs = selectionSort' 0 xs
  where selectionSort' i ls | i == length ls - 1 = ls
                            | otherwise = selectionSort' (i+1) (swap i (indexOfMinimum i ls) ls)

-- END
