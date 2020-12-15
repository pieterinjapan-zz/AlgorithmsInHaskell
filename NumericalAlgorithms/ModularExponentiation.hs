{-
Author  : Pieter van Wyk
Created : 2020-12-14
Updated : 2020-12-15

Implimentation of fast modular exponentiation algorithm
-}
module ModularExponentiation where
import BinaryRepresentation

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

-- 2) algorithm using binary representation and squaring

-- for given integers b and k, compute b^(2^k) modular to m
squareMod :: Integer -> Integer -> Integer -> Integer
squareMod b 0 m = mod b m
squareMod b k m = let aux = squareMod b (k - 1) m
                  in mod (aux * aux) m

-- fast modular exponentiation using externally defined binary representation
modPowBinary :: Integer -> Integer -> Integer -> Integer
modPowBinary _ 0 m = mod 1 m
modPowBinary b e m = foldl aux 1 binary_e
  where binary_e = intToBinary e
        aux v (fac,k) | fac == One = mod (v * squareMod b k m) m
                      | otherwise = mod v m

-- fast modular exponentiation using internally defined binary representation
modPowBinary' :: Integer -> Integer -> Integer -> Integer
modPowBinary' _ 0 m = mod 1 m
modPowBinary' b e m = aux e base 1
  where base = floor $ logBase 2 (fromIntegral e)
        aux e 0 acc = if e == 1 then mod (acc * b) m else acc
        aux e k acc = if div e (2^k) == 1
                          then mod (aux (e - 2^k) (k - 1) (acc * squareMod b k m)) m
                          else mod (aux e (k - 1) acc) m

-- END
