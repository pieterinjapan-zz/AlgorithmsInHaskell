{-
Author  : Pieter van Wyk
Created : 2020-12-14
Updated : 2020-12-14

Implimentation of an algorithm for fast modular exponentiation
-}
module ModularExponentiation where
import BinaryRepresentation

import Test.QuickCheck

-- Main Functions --
--------------------

-- 1) algorithm using standard fast-exponentiation
modPow :: Integral a => a -> a -> a -> a
modPow _ 0 m = mod 1 m
modPow b e m | even e = even_pow
             | otherwise = mod (mod_b * even_pow) m
  where mod_b = mod b m
        sqr_b = mod (mod_b * mod_b) m
        even_pow = modPow sqr_b (div e 2) m

prop_modPow_00 :: Integer -> Integer -> Integer -> Bool
prop_modPow_00 b e m = modPow b e' m' == mod (b^e') m'
  where e' = max 2 (abs e)
        m' = max 2 (abs m)

-- 2) algorithm using binary representation and squaring
-- TODO

-- END
