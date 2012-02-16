module Main (main, euler35) where
import EulerUtil

primes = filter oddDigits . takeWhile (<1000000) $ primesTME()
isPrime p = p `elem` primes

oddDigits n = (not . elem True . map even . intToList) $ n

isCirc p = isCirc' (cycle' p)
	where
		isCirc' x
			| x == p = True
			| isPrime x = isCirc' (cycle' x)
			| otherwise = False
		
		cycle' x = read ((last s): (init s)) :: Integer
			where s = show x
euler35 = length  (2:[p | p<-primes, isCirc p])
main = print euler35
