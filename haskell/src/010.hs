{-
 - Project Euler 10
 -
 - The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 - Find the sum of all the primes below two million.
 -}
module Euler10 (euler10) where
import Numbers(primesTME)

euler10 n = sum . takeWhile (<n) $ primesTME
answer =  euler10 2000000
