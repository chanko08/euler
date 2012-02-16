{-
 - Project Euler 49
 -
 - The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
 - increases by 3330, is unusual in two ways:
 -
 -      (i) each of the three terms are prime, and, 
 -      (ii) each of the 4-digit numbers are permutations of one another.
 -
 - There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes,
 - exhibiting this property, but there is one other 4-digit increasing sequence.
 -
 - What 12-digit number do you form by concatenating the three terms in this
 - sequence?
 -}
module Main(euler49, main) where
import Numbers
import Data.List(sort)

primes = takeWhile (\x -> x < 10000) . dropWhile (\x -> x < 1000) $ primesTME
candidates = [(a,b) | a <-primes, b <- dropWhile (<=a) primes, sort (show a) == sort (show b)]
formatAnswer (a,b,c) = show a ++ show b ++ show c

euler49 = formatAnswer . head . drop 1 $ [(a,b,c) | (a,b)<-candidates, let c = 2*b - a, sort (show c) == sort (show a), c `elem` primes]
main = print euler49
