{-
 - Project Euler 35
 -
 - The number, 197, is called a circular prime because all rotations of the
 - digits: 197, 971, and 719, are themselves prime.
 -
 - There are thirteen such primes below 100:
 - 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
 -
 - How many circular primes are there below one million?
 -}
module Euler35 (euler35) where
import Util(intToList)
import Numbers(primesTME)

primes = map fromIntegral . filter oddDigits . takeWhile (<1000000) $ primesTME
isPrime p = p `elem` primes

oddDigits n = notElem True . map even . intToList $ n

isCirc p = isCirc' (cycle' p) where
    isCirc' x
        | x == p = True
        | isPrime x = isCirc' (cycle' x)
        | otherwise = False

    cycle' x = read (last s: init s) :: Integer where
        s = show x
euler35 = length  (2:[p | p<-primes, isCirc p])

answer = euler35
