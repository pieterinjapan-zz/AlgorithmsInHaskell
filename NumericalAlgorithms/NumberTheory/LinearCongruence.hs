{-
Author  : Pieter van Wyk
Created : 2020-12-09
Updated : 2020-12-14

Implimentation of algorithm for solving set of
linear congruence equations using the Chinese remainder theorem
-}
module LinearCongruence where
import GCDEuclid

-- Main Functions --
--------------------

-- solution of set of linear congruence equations of the form n = r_a (mod a), n = r_b (mod b)
-- 1) use extended Euclids algorithm to find (x,y) s.t. a*x + b*y = gcd a b
-- 2) then (r_a*b*y + r_b*a*x) / gcd a b solves the congruence equations
solveCon :: (Int,Int) -> (Int,Int) -> Int
solveCon (r_a,a) (r_b,b) | mod (r_b - r_a) (gcd a b) /= 0 = error "no solution"
                         | otherwise = let (d, x, y) = extended_gcd a b
                                           n = div (r_a*b*y + r_b*a*x) d
                                       in mod n (a * b)

-- solution of general set of linear congruence equations,
-- assuming all moduli are coprime
solveGenCon :: [(Int,Int)] -> Int
solveGenCon = snd . solveGenCon'
  where solveGenCon' ((r_a,a):ps) | null ps = (a,r_a)
                                  | otherwise = let (prod',n') = solveGenCon' ps
                                                    (_,x,y) = extended_gcd prod' a
                                                    prod = prod'*a
                                                    n = mod (r_a*prod'*x + n'*a*y) prod
                                                in (prod, n)

-- END
