{-
 - Project Euler 5
 -
 - 2520 is the smallest number that can be divided by each of the numbers from
 - 1 to 10 without any remainder. 
 -
 - What is the smallest positive number that is evenly divisible by all of the
 - numbers from 1 to 20?
 -}
module Euler5 (euler5) where

euler5 n = foldl lcm 1 [1..n]
answer = euler5 20
