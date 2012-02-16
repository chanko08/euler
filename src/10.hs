{-
 - Project Euler 10
 -
 - The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 - Find the sum of all the primes below two million.
 -}
module Main (main, euler10) where
import EulerUtil

euler10 = sum $ takeWhile (<2000000) $ primesTME()
main = print euler10
