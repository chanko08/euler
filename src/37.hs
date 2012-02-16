module Main (main, euler37) where
import Data.List
import EulerUtil

primes = takeWhile (<1000000) $ primesTME()


digitsToInt xs = digitsToInt' xs ((length xs) - 1)
	where
		digitsToInt' [] _ = 0
		digitsToInt' (y:ys) j = y*(10^j) + (digitsToInt' ys (j-1)) 


truncLeftPrime n
	| elem True . map (\x -> elem x n) $ "4680" = False
	| otherwise = not . elem False . map (\x -> elem (read x::Int) primes) . init . tails $ n

truncRightPrime n
	| elem True . map (\x -> elem x n) $ "4680" = False
	| otherwise = not . elem False . map (\x -> elem (read (reverse x)::Int) primes) . init . tails . reverse $ n

euler37 = sum $ drop 4 [x | x<-primes, let n = show x, truncLeftPrime n, truncRightPrime n]
main = print euler37
