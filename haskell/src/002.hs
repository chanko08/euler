{-
 - Project Euler 2
 -
 - Each new term in the Fibonacci sequence is generated by adding the previous
 - two terms. By starting with 1 and 2, the first 10 terms will be: 
 -
 - 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ... 
 -
 - By considering the terms in the Fibonacci sequence whose values do not exceed
 - four million, find the sum of the even-valued terms.
 -}
module Euler2 (main, euler2) where
import Numbers(fibs, divides)
euler2 limit = sum . filter (divides 2) . takeWhile (<limit) $ fibs
answer = euler2 4000000
