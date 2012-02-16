module Main (main, euler27)
where
import EulerUtil


consecPrimes (a, b) = consecPrimes' 0
	where
		consecPrimes' x
			| (x^2 + a*x+b) `elem` primes = (x^2 + a*x + b) : consecPrimes' (x+1)
			| otherwise = []

primes = takeWhile (<2000) $ primesTME()

euler27 =(product . drop 1 . maximum) [[numP (a,b), a, b] | (a,b) <- search_space]
	where
		search_space = [(a,b) |  b<-primes, a<-[-999,-997..999], 2 < b, b < 1000, (1+a+b) `elem` primes]
		numP = length . consecPrimes

main = print euler27

