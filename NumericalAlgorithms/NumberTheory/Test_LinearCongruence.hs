{-
Author  : Pieter van Wyk
Created : 2020-12-09
Updated : 2020-12-14

Property tests for algorithm for solving set of
linear congruence equations using the Chinese remainder theorem
-}
import LinearCongruence
import PrimeNumbers
import Test.QuickCheck
import System.Random

-- Helper Functions --
----------------------

-- helper function to determine if all elements in a list are coprime
checkCoprime :: [Int] -> Bool
checkCoprime [] = True
checkCoprime [_] = True
checkCoprime (n:ns) = all (\x -> gcd n x == 1) ns && checkCoprime ns

-- generate random list of integers
randomList :: Int -> [Int]
randomList seed = randoms (mkStdGen seed) :: [Int]

-- generate test list for solveGenCon using random list
genTestList :: Int -> [(Int,Int)]
genTestList seed = [(mod r_a a,a) | (r_a,a) <- zip remainders factors]
  where randomlist = randomList seed
        factors = primeFact (max 2 (mod (head randomlist) 100000))
        remainders = tail randomlist

-- check if a given number solves a set of congruence equations
checkSolveCon :: Int -> [(Int,Int)] -> Bool
checkSolveCon _ [] = True
checkSolveCon n ((r_a,a):ps) = mod n a == r_a && checkSolveCon n ps

-- Property Tests --
--------------------

-- check if solutions found by solveCon satisfy the congruence equations
prop_solveCon_00 :: (Int,Int) -> (Int,Int) -> Int -> Bool
prop_solveCon_00 (r_a,a) (r_b,b) m = checkSolveCon n [(r_a',a'),(r_b',b')]
  where a' = max (abs a) 1 -- use positive integer greater than 1 for a', b'
        b' = max (abs b) 1
        d = gcd a' b'
        r_a' = mod r_a a'
        r_b' = mod ((max m 1)*d + r_a') b' -- ensure d divides r_b' - r_a'
        n = solveCon (r_a',a') (r_b',b')

-- check if all possible solutions generated from a*x + b*y = r_b - r_a
-- satisfy the congruence equations
prop_solveCon_01 :: (Int,Int) -> (Int,Int) -> Int -> Int -> Bool
prop_solveCon_01 (r_a,a) (r_b,b) m t = checkSolveCon n_t [(r_a',a'),(r_b',b')]
  where a' = max (abs a) 1
        b' = max (abs b) 1
        d = gcd a' b'
        r_a' = mod r_a a'
        r_b' = mod ((max m 1)*d + r_a') b'
        n = solveCon (r_a',a') (r_b',b')
        n_t = n - div (t * a' * b') d

-- check if different solutions of congruence equations satisfy
-- Chinese remainder Theorem
prop_solveCon_02 :: (Int,Int) -> (Int,Int) -> Int -> Int -> Int -> Bool
prop_solveCon_02 (r_a,a) (r_b,b) m t1 t2 = mod (n_t1 - n_t2) (lcm a' b') == 0
  where a' = max (abs a) 1
        b' = max (abs b) 1
        d = gcd a' b'
        r_a' = mod r_a a'
        r_b' = mod ((max m 1)*d + r_a') b'
        n = solveCon (r_a',a') (r_b',b')
        n_t1 = n - div (t1 * a' * b') d
        n_t2 = n - div (t2 * a' * b') d

-- check if solution from solveGenCon solves congruence equations
prop_solveGenCon_00 :: Int -> Bool
prop_solveGenCon_00 seed = checkSolveCon n_test test_ls
  where test_ls = genTestList seed
        n_test = solveGenCon test_ls

-- check if solution from solveGenCon is equal to itself modular to the
-- product of all the moduli
prop_solveGenCon_01 :: Int -> Bool
prop_solveGenCon_01 seed = n_test == mod n_test prod
  where test_ls = genTestList seed
        prod = product [a | (_,a) <- test_ls]
        n_test = solveGenCon test_ls

main = do
  quickCheck (withMaxSuccess 10000 prop_solveCon_00)
  quickCheck (withMaxSuccess 10000 prop_solveCon_01)
  quickCheck (withMaxSuccess 10000 prop_solveCon_02)
  quickCheck (withMaxSuccess 10000 prop_solveGenCon_00)
  quickCheck (withMaxSuccess 10000 prop_solveGenCon_01)
