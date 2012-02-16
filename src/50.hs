{-
 - Project Euler 50
 -
 - The prime 41, can be written as the sum of six consecutive primes:
 - 41 = 2 + 3 + 5 + 7 + 11 + 13
 -
 - This is the longest sum of consecutive primes that adds to a prime below
 - one-hundred.
 -
 - The longest sum of consecutive primes below one-thousand that adds to a
 - prime, contains 21 terms, and is equal to 953.
 -
 - Which prime, below one-million, can be written as the sum of the most
 - consecutive primes?
 -}
module Main(euler50, main) where
import Numbers
import Data.List(tails, maximumBy)
import Data.Set(fromList, member)

big = 1000000
primes = takeWhile (<big) $ primesTME
p_set = fromList primes


findPrimeSums _ [] = []
findPrimeSums maxNum (x:xs) = findPrimeSums' 1 x xs where
    findPrimeSums' c t [] = []
    findPrimeSums' c t (x:xs)
        | t + x > maxNum = []
        | (member (t+x) p_set) = (t + x, c + 1): (findPrimeSums' (c+1) (t+x) xs)
        | otherwise = findPrimeSums' (c+1) (t+x) xs
    
sorting (p,a) (q,b)
    | a > b = GT
    | a < b = LT
    | a == b = compare p q

euler50 = maximumBy sorting . concat . map (findPrimeSums big) . tails $ primes
main = print euler50
