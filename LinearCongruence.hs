{-
Author  : Pieter van Wyk
Created : 2020-12-09
Updated : 2020-12-10

Implimentation of algorithm for solving set of
linear congruence equations of the form n = r_a (mod a),
                                        n = r_b (mod b).
-}
module LinearCongruence where
import GCDEuclid

-- Main Functions --
--------------------

-- 1) use extended Euclids algorithm to find (x,y) s.t. a*x + b*y = gcd a b
-- 2) then (r_a*b*y + r_b*a*x) / gcd a b solves the congruence equations
solveCon :: Int -> Int -> Int -> Int -> Int
solveCon a b r_a r_b | mod (r_b - r_a) (gcd a b) /= 0 = error "no solution"
                     | otherwise = let (d, x, y) = extended_gcd a b
                                   in div (r_a*b*y + r_b*a*x) d

-- END
