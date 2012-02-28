{- 
 - Project Euler 1
 -
 - If we list all the natural numbers below 10 that are multiples of 3 or 5, we
 - get 3, 5, 6 and 9. The sum of these multiples is 23.
 -
 - Find the sum of all the multiples of 3 or 5 below 1000.
 -}
module Euler1 (euler1) where
import Numbers(divides)

euler1 xs = sum . filter (\x -> divides 3 x || divides 5 x) $ xs

answer = euler1 [1..999]
