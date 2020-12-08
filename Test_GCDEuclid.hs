{-
Author  : Pieter van Wyk
Created : 2020-12-08
Updated : 2020-12-08

Unit tests of extended Euclid algorithm
-}
import GCDEuclid
import Test.QuickCheck

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

-- checking result for gcd
prop_gcd_00 :: Int -> Int -> Bool
prop_gcd_00 a b = gcdE a' b' == d
  where a' = abs a
        b' = max (abs b) 1
        (d,_,_) = extended_gcd a' b'

-- checking result for certificate
prop_gcd_01 :: Int -> Int -> Bool
prop_gcd_01 a b = cert a' b' == (x,y)
  where a' = abs a
        b' = max (abs b) 1
        (_,x,y) = extended_gcd a' b'

-- check that certificate is valid
prop_gcd_02 :: Int -> Int -> Bool
prop_gcd_02 a b = d == a'*x + b'*y
  where a' = abs a
        b' = max (abs b) 1
        (d,x,y) = extended_gcd a' b'

-- check that gcd divides its parameters
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
