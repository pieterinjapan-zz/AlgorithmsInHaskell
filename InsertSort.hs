{-
Author  : Pieter van Wyk
Created : 2020-12-02
Updated : 2020-12-08

Implimentation of insert sort
-}
module InsertSort where

-- Main Functions --
--------------------

-- insert element into sorted list
insert :: Ord a => a -> [a] -> [a]
insert x []     = [x]
insert x (l:ls) | x > l = l : (insert x ls)
                | otherwise = x:l:ls

-- sort list with insert sort algorithm
insertSort :: Ord a => [a] -> [a]
insertSort [] = []
insertSort (l:ls) = insert l (insertSort ls)

-- END

