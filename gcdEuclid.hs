{-
Author  : Pieter van Wyk
Created : 2020-12-02
Updated : 2020-12-02

Implimentation of Extended Euclid Algorithm
-}

import Test.QuickCheck

-- Main Functions --
--------------------

-- extended Euclid algorithm implimentation.
-- returns gcd as well as certificate x,y
-- defined by gcd a b = a*x + b*y
extended_gcd :: Int -> Int -> (Int, Int, Int)
extended_gcd a 0 = (a, 1, 0)
extended_gcd a b = let (d, p, q) = extended_gcd b (mod a b)
                   in (d, q, p - q * (div a b))

-- Unit Tests --
----------------

-- test helper function 1
-- isolated calculation of gcd
-- by Euclids algorithm
gcdE :: Int -> Int -> Int
gcdE x 0 = x
gcdE x y = gcdE y (mod x y)

-- test helper function 2
-- isolated calculation of certificate x,y
-- defined by gcd a b = a*x + b*y
cert :: Int -> Int -> (Int, Int)
cert a 0 = (1,0)
cert a b = (q, p - (div a b)*q)
 where (p,q) = cert b (mod a b)
 
prop_gcd_00 :: Int -> Int -> Bool
prop_gcd_00 a b = gcdE a' b' == d
  where a' = abs a
        b' = max (abs b) 1
        (d,_,_) = extended_gcd a' b'

prop_gcd_01 :: Int -> Int -> Bool
prop_gcd_01 a b = cert a' b' == (x,y)
  where a' = abs a
        b' = max (abs b) 1
        (_,x,y) = extended_gcd a' b'

prop_gcd_02 :: Int -> Int -> Bool
prop_gcd_02 a b = d == a'*x + b'*y
  where a' = abs a
        b' = max (abs b) 1
        (d,x,y) = extended_gcd a' b'

prop_gcd_03 :: Int -> Int -> Bool
prop_gcd_03 a b = mod a' d == 0 && mod b' d == 0
  where a' = abs a
        b' = max (abs b) 1
        (d,_,_) = extended_gcd a' b'

main = do
  quickCheck (withMaxSuccess 10000 prop_gcd_00)
  quickCheck (withMaxSuccess 10000 prop_gcd_01)
  quickCheck (withMaxSuccess 10000 prop_gcd_02)
  quickCheck (withMaxSuccess 10000 prop_gcd_03)
