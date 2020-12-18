{-
Author  : Pieter van Wyk
Created : 2020-12-18
Updated : 2020-12-18

Implimentation of prime number test and generation algorithms
-}
module PrimeNumbers where
import Test.QuickCheck

-- Helper Functions --
----------------------

-- divide out a factor from a number
divideOut :: Integral a => a -> a -> a
divideOut n num | n /= 1 && mod num n == 0 = divideOut n (div num n)
                | otherwise = num

-- generate list of odd prime factors (without repetition) of given number num
prime_odd :: Integral a => a -> a -> [a] -> [a]
prime_odd num n acc_ls | num == 1       = acc_ls
                       | mod num n == 0 = prime_odd num' n acc_ls'
                       | otherwise      = prime_odd num (n+2) acc_ls
                       where
                         num'    = divideOut n num
                         acc_ls' = acc_ls ++ [n]

-- Main Functions --
--------------------

-- generate list of prime factors (without repetition) of given number
primeFact :: Integral a => a -> [a]
primeFact num | num < 0 = primeFact (-num)
              | otherwise = primeFact' num []
  where primeFact' num acc | num == 1 = acc
                           | mod num 2 == 0 = let acc' = if elem 2 acc then acc else 2:acc
                                              in primeFact' (div num 2) acc'
                           | otherwise = prime_odd num 3 acc

-- test if a given integer is prime
isPrime :: Integral a => a -> Bool
isPrime n | n <= 1 = False
          | otherwise = aux test_nums
  where sqrt_n = floor $ sqrt (fromIntegral n)
        test_nums = [2 .. sqrt_n]
        aux [] = True
        aux (test_num:test_nums) | mod n test_num == 0 = False
                                 | otherwise = let test_nums' = filter (\x -> mod x test_num /= 0) test_nums
                                               in aux test_nums'

-- END
