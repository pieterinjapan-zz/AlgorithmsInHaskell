{-
Author  : Pieter van Wyk
Created : 2020-12-14
Updated : 2020-12-14

Implimentation of an algorithm for converting integers in decimal
to binary representation
-}
module BinaryRepresentation where

-- Data Type Definitions --
---------------------------
data Factor = Zero | One
  deriving (Show, Eq)
type BinaryTerm = (Factor,Integer)
type Binary = [BinaryTerm]

-- Main Functions --
--------------------

-- convert integer to binary representation
intToBinary :: Integer -> Binary
intToBinary 0 = [(Zero,0)]
intToBinary num = intToBinary' num base []
  where base = floor $ logBase 2 (fromIntegral num)
        intToBinary' num base acc | base == 0 = if num == 0 then acc ++ [(Zero,base)] else acc ++ [(One,base)]
                                  | div num (2^base) == 1 = intToBinary' (num - 2^base) (base - 1) (acc ++ [(One,base)])
                                  | otherwise = intToBinary' num (base - 1) (acc ++ [(Zero,base)])

-- convert binary representation to integer
binaryToInt :: Binary -> Integer
binaryToInt [] = 0
binaryToInt (b:bs) | fst b == One = 2^(snd b) + binaryToInt bs
                   | otherwise = binaryToInt bs

-- END
