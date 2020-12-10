{-
Author  : Pieter van Wyk
Created : 2020-12-09
Updated : 2020-12-10

Property tests for algorithm for solving set of
linear congruence equations of the form n = r_a (mod a),
                                        n = r_b (mod b).
-}
import LinearCongruence
import Test.QuickCheck

-- Property Tests --
--------------------

-- check if solutions found by solveCon satisfy the congruence equations
prop_lincon_00 :: Int -> Int -> Int -> Int -> Int -> Bool
prop_lincon_00 a b r_a r_b m = mod (n - r_a) a' == 0 && mod (n - r_b') b' == 0
  where a' = max (abs a) 1 -- use positive integer greater than 0 for a', b'
        b' = max (abs b) 1
        d = gcd a' b'
        r_b' = (max m 1)*d + r_a -- ensure d divides r_b' - r_a
        n = solveCon a' b' r_a r_b'

-- check if all possible solutions generated from a*x + b*y = r_b - r_a
-- satisfy the congruence equations
prop_lincon_01 :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_lincon_01 a b r_a r_b m t = mod (n_t - r_a) a' == 0 && mod (n_t - r_b') b' == 0
  where a' = max (abs a) 1
        b' = max (abs b) 1
        d = gcd a' b'
        r_b' = (max m 1)*d + r_a
        n = solveCon a' b' r_a r_b'
        n_t = n - div (t * a' * b') d

-- check if different solutions of congruence equations satisfy
-- Chinese remainder Theorem
prop_lincon_02 :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_lincon_02 a b r_a r_b m t1 t2 = mod (n_t1 - n_t2) (lcm a' b') == 0
  where a' = max (abs a) 1
        b' = max (abs b) 1
        d = gcd a' b'
        r_b' = (max m 1)*d + r_a
        n = solveCon a' b' r_a r_b'
        n_t1 = n - div (t1 * a' * b') d
        n_t2 = n - div (t2 * a' * b') d

main = do
  quickCheck (withMaxSuccess 10000 prop_lincon_00)
  quickCheck (withMaxSuccess 10000 prop_lincon_01)
  quickCheck (withMaxSuccess 10000 prop_lincon_02)
