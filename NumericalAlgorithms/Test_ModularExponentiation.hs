{-
Author  : Pieter van Wyk
Created : 2020-12-15
Updated : 2020-12-18

Property tests for fast modular exponentiation algorithm
-}
import ModularExponentiation
import Test.QuickCheck
import PrimeNumbers

-- Property Tests --
--------------------

prop_modPow_00 :: Integer -> Integer -> Integer -> Bool
prop_modPow_00 b e m = modPow b e' m' == mod (b^e') m'
  where e' = max 2 (abs e)
        m' = max 2 (abs m)

prop_squareMod_00 :: Integer -> Integer -> Integer -> Bool
prop_squareMod_00 b k m = squareMod b' k' m' == mod (b'^(2^k')) m'
  where b' = max 1 (abs b)
        k' = mod (abs k) 20
        m' = max 1 (abs b)

prop_squareMod_01 :: Integer -> Integer -> Integer -> Bool
prop_squareMod_01 b k m = squareMod b' k' m' == modPow b' (2^k') m'
  where b' = max 1 (abs b)
        k' = mod (abs k) 20
        m' = max 1 (abs b)

prop_modPowBinary_00 :: Integer -> Integer -> Integer -> Bool
prop_modPowBinary_00 b e m = modPowBinary b e' m' == mod (b^e') m'
  where e' = max 2 (abs e)
        m' = max 2 (abs m)

prop_modPowBinary_01 :: Integer -> Integer -> Integer -> Bool
prop_modPowBinary_01 b e m = modPowBinary b e' m' == modPow b e' m'
  where e' = max 2 (abs e)
        m' = max 2 (abs m)

prop_modPowBinary_02 :: Integer -> Integer -> Integer -> Bool
prop_modPowBinary_02 b e m = modPowBinary b e' m' == modPowBinary' b e' m'
  where e' = max 2 (abs e)
        m' = max 2 (abs m)

-- check that Fermat's little theorem gives same exponent as regular
-- exponentiation modular to a prime number above 3
prop_modPowPrime_00 :: Integer -> Integer -> Integer -> Bool
prop_modPowPrime_00 b e num = map (modPowPrime b' e') primes == map (modPowBinary b' e') primes
  where b' = max 2 (abs b)
        e' = max 2 (abs e)
        primes = primeFact (max 3 (abs num))

main = do
  quickCheck (withMaxSuccess 10000 prop_modPow_00)
  quickCheck (withMaxSuccess 10000 prop_squareMod_00)
  quickCheck (withMaxSuccess 10000 prop_squareMod_01)
  quickCheck (withMaxSuccess 10000 prop_modPowBinary_00)
  quickCheck (withMaxSuccess 10000 prop_modPowBinary_01)
  quickCheck (withMaxSuccess 10000 prop_modPowBinary_02)
  quickCheck (withMaxSuccess 10000 prop_modPowPrime_00)
