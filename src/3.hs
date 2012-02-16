{-
 - Project Euler 3
 -
 - The prime factors of 13195 are 5, 7, 13 and 29. 
 - What is the largest prime factor of the number 600851475143 ?
 -}
module Main (main, euler3) where
import EulerUtil

euler3 = head $ filter (\x -> 600851475143 `mod` x ==0) $ reverse $ takeWhile( <floor(sqrt(600851475143)) ) $ primesTME()
main = print euler3
