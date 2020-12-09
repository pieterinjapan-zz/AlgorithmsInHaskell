{-
Author  : Pieter van Wyk
Created : 2020-12-09
Updated : 2020-12-09

Implimentation of algorithm for solving linear Diophantine
equation of the form a*x + b*y = c
-}
module LinearDiophantine where
import GCDEuclid

-- Main Functions --
--------------------

solveDio :: Int -> Int -> Int -> (Int, Int)
solveDio a b c | mod c (gcd a b) /= 0 = error "no solution"
               | otherwise = let d = gcd a b
                                 m = div c d
                                 (_,x,y) = extended_gcd (div a d) (div b d)
                             in (m*x, m*y)

-- END
