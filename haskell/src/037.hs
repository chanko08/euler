{-
 - Project Euler 37
 -
 - The number 3797 has an interesting property. Being prime itself, it is
 - possible to continuously remove digits from left to right, and remain prime 
 - at each stage: 3797, 797, 97, and 7. Similarly we can work from right to 
 - left: 3797, 379, 37, and 3.
 -
 - Find the sum of the only eleven primes that are both truncatable from left to
 - right and right to left.
 - 
 - NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
 -}
module Euler37 (euler37) where
import Data.List
import Numbers(primesTME)

primes = takeWhile (<1000000) primesTME


digitsToInt (x:xs) = x*10^l + digitsToInt xs where
    l = length xs + 1

evenDigits n = elem True . map (`elem` n) $ "4680"

truncLeftPrime n
	| evenDigits n = False
	| otherwise = notElem False . map (\x -> (read x::Int) `elem` primes) . init . tails $ n

truncRightPrime n
	| evenDigits n = False
	| otherwise = notElem False . map (\x ->(read (reverse x)::Int) `elem` primes) . init . tails . reverse $ n

euler37 = sum $ drop 4 [x | x<-primes, let n = show x, truncLeftPrime n, truncRightPrime n]
answer = euler37
