{-
Author  : Pieter van Wyk
Created : 2020-12-08
Updated : 2020-12-08

Implementation of binary search algorithm
-}
module BinarySearch where

-- Main Functions --
--------------------

binarySearch' :: Ord a => Int -> Int -> [a] -> a -> Maybe Int
binarySearch' _ _ [] _ = Nothing
binarySearch' minEl maxEl ls x | maxEl < minEl  = Nothing
                               | x == ls!!guess = Just guess
                               | x <  ls!!guess = binarySearch' minEl maxEl' ls x
                               | otherwise      = binarySearch' minEl' maxEl ls x
                               where guess = round $ 0.5 * fromIntegral (minEl + maxEl)
                                     minEl' = guess + 1
                                     maxEl' = guess - 1

binarySearch :: Ord a => [a] -> a -> Maybe Int
binarySearch ls = binarySearch' 0 (length ls - 1) ls

-- END
