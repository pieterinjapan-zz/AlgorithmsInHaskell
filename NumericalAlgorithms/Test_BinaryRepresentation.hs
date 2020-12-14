{-
Author  : Pieter van Wyk
Created : 2020-12-14
Updated : 2020-12-14

Property tests for algorithm for converting integers in decimal
to binary representation
-}
import BinaryRepresentation
import System.Random
import Test.QuickCheck

-- Property Tests --
--------------------

-- check that binaryToInt . intToBinary is identity function on Integer type
prop_intToBinary_00 :: Integer -> Bool
prop_intToBinary_00 num = (binaryToInt . intToBinary) num' == num'
  where num' = abs num

-- check that intToBinary . binaryToInt is identity function on Binary type
prop_intToBinary_01 :: Integer -> Int -> Bool
prop_intToBinary_01 num seed = (intToBinary . binaryToInt) bin' == bin'
  where num' = abs num
        random_lst = randoms (mkStdGen seed) :: [Int]
        bin = [(toFactor (mod ran 2),n) | (ran,n) <- zip random_lst (reverse [0 .. num'])]
        bin' = (One,num'+1):bin -- ensure first element of generated bnary number is not zero
        toFactor x | x == 0 = Zero
                   | otherwise = One

main = do
  quickCheck (withMaxSuccess 10000 prop_intToBinary_00)
  quickCheck (withMaxSuccess 10000 prop_intToBinary_01)
