{-
 - Project Euler 3
 -
 - The prime factors of 13195 are 5, 7, 13 and 29. 
 - What is the largest prime factor of the number 600851475143 ?
 -}
module Euler3 (euler3) where
import Numbers(primesTME, divides)

euler3 n = head . filter (`divides` n) . reverse . takeWhile (< limit ) $ primesTME where
    limit = floor . sqrt . fromIntegral $ n
answer = euler3 600851475143
