{-
 - Project Euler 36
 -
 - The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
 -
 - Find the sum of all numbers, less than one million, which are palindromic in
 - base 10 and base 2.
 - 
 - (Please note that the palindromic number, in either base, may not include
 - leading zeros.)
 -}
module Main (main, euler36) where
import EulerUtil


base2 x = base2' x maxPow
	where 
		maxPow = last $ takeWhile (\i -> (x `div` 2^i) /= 0) [0..]
		base2' c p
			| c == 0 = (take (p+1) . cycle) $ [0]
			| (c - 2^p) < 0 = 0: (base2' c (p-1))
			| otherwise = 1: (base2' (c-2^p) (p-1))
		

isBase2Pal x = n == reverse n
	where n = base2 x

euler36 = sum [x | x<-[1..1000000], isPalindrome x, isBase2Pal x ] 
main = print euler36
