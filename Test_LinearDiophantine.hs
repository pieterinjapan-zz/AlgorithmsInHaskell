{-
Author  : Pieter van Wyk
Created : 2020-12-09
Updated : 2020-12-09

Property tests for algorithm for solving linear Diophantine
equation of the form a*x + b*y = c
-}
import LinearDiophantine
import Test.QuickCheck

-- Property Test --
-------------------

-- check if solveDio generates solution (x,y) for equation a*x + b*y = c
prop_lindio_00 :: Int -> Int -> Int -> Bool
prop_lindio_00 a b c = let (x, y) = solveDio a b' c'
                       in a*x + b'*y == c'
  where  b' = (max b 1)
         c' = (max c 1) * (gcd a b)

-- check if (x,y) is a solution for a*x + b*y = c,
-- then (x - t*q, y + t*p) is also a solution, where
-- t is an integer and p = a / gcd a b, q = b / gcd a b
prop_lindio_01 :: Int -> Int -> Int -> Int -> Bool
prop_lindio_01 a b c t = let (x, y) = solveDio a b' c'
                         in a*(x - t*q) + b'*(y + t*p) == c'
  where  b' = (max b 1)
         d = gcd a b'
         p = div a d
         q = div b' d
         c' = (max c 1) * d

main = do
  quickCheck (withMaxSuccess 10000 prop_lindio_00)
  quickCheck (withMaxSuccess 10000 prop_lindio_01)
