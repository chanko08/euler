module Main (main, euler47) where
import Data.List
import EulerUtil

euler47 = head . filter (all (==4) . map snd) . map (take 4) . tails . zip [2..] . map numPrimeFacs $ [13434340..]


primes = take 500 $ primesTME()

ncombo 1 xs = [[x] | x<-xs]
ncombo n xs = [x:y | x<-xs, y <- ncombo (n-1) . filter(>x) $ xs]

numPrimeFacs n = factor n primes
	where
		factor _ [] = 0 
		factor m (p:ps)
			| m == 1  = 0 
			| m `mod` p ==  0 = 1 + (factor (divideOut m p) ps)
			| otherwise = factor m ps

divideOut n d
	| n `mod` d /= 0 = n
	| otherwise = divideOut (n `div` d) d 
main = print euler47
