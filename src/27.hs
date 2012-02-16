{-
 - Project Euler 27
 -
 - Euler published the remarkable quadratic formula:
 -      n² + n + 41
 - 
 - It turns out that the formula will produce 40 primes for the consecutive
 - values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is
 - divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly
 - divisible by 41.
 -
 - Using computers, the incredible formula  n² − 79n + 1601 was discovered,
 - which produces 80 primes for the consecutive values n = 0 to 79. The product
 - of the coefficients, −79 and 1601, is −126479.
 -
 - Considering quadratics of the form:
 -      n² + an + b, where |a| < 1000 and |b| < 1000
 -      where |n| is the modulus/absolute value of n
 - 
 - e.g. |11| = 11 and |−4| = 4
 -
 - Find the product of the coefficients, a and b, for the quadratic expression
 - that produces the maximum number of primes for consecutive values of n,
 - starting with n = 0.
 -}
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

