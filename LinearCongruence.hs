{-
Author  : Pieter van Wyk
Created : 2020-12-09
Updated : 2020-12-09

Implimentation of algorithm for solving set of
linear congruence equations of the form n = r_a (mod a),
                                        n = r_b (mod b).
-}
module LinearCongruence where
import LinearDiophantine

import Test.QuickCheck

-- Main Functions --
--------------------

solveCon :: Int -> Int -> Int -> Int -> (Int, Int)
solveCon a b r_a r_b | mod (r_a - r_b) (gcd a b) /= 0 = error "no solution"
--solveCon a b r_a r_b | gcd a b /= 1 = error "no solution"
                     | otherwise = let (x,y) = solveDio a b (r_b - r_a)
                                   in (x*a + r_a, -y*b + r_b)

-- check if solutions found by solveCon satisfy the congruence equations

-- check if different solutions of congruence equations satisfy
-- Chinese remainder Theorem


-- END
