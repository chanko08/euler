{-
 - Project Euler 46
 -
 - It was proposed by Christian Goldbach that every odd composite number can be
 - written as the sum of a prime and twice a square.
 -
 - 9 = 7 + 2×12
 - 15 = 7 + 2×22
 - 21 = 3 + 2×32
 - 25 = 7 + 2×32
 - 27 = 19 + 2×22
 - 33 = 31 + 2×12
 -
 - It turns out that the conjecture was false.
 -
 - What is the smallest odd composite that cannot be written as the sum of a
 - prime and twice a square?
 -}
module Euler46 (euler46) where
import Numbers(primesTME)
import qualified Data.Set as Set

conject n = (>0) . length . filter isPrime  $ sqrs where 
    sqrs = takeWhile (>0) . map (\x -> n-2*x*x) $ [1..]

primes = Set.fromList . take 6000 $ primesTME
isPrime x = x `Set.member` primes

euler46 = head . filter (not . conject) . filter (not . isPrime) $ [9,11..]
answer = euler46
