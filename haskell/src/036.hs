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
module Euler36 (euler36) where
import Numeric(showIntAtBase)
import Data.Char(intToDigit)
import Numbers(isPalindrome)


base2 x = showIntAtBase 2 intToDigit x ""		

isBase2Pal x = base2 x == reverse (base2 x)

euler36 = sum [x | x<-[1..1000000], isPalindrome x, isBase2Pal x ] 
answer = euler36
