{-
Author  : Pieter van Wyk
Created : 2020-12-02
Updated : 2020-12-08

Implimentation of extended Euclid algorithm
-}
module GCDEuclid where

-- Main Functions --
--------------------

-- extended Euclid algorithm implimentation.
-- returns gcd as well as certificate x,y
-- defined by gcd a b = a*x + b*y
extended_gcd :: Int -> Int -> (Int, Int, Int)
extended_gcd a 0 = (a, 1, 0)
extended_gcd a b = let (d, p, q) = extended_gcd b (mod a b)
                   in (d, q, p - q * (div a b))

-- END
