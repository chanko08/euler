{-
 - Project Euler 4
 -
 - A palindromic number reads the same both ways. The largest palindrome made 
 - from the product of two 2-digit numbers is 9009 = 91 × 99. 
 -
 - Find the largest palindrome made from the product of two 3-digit numbers.
 -}
module Euler4 (euler4) where
import Numbers(isPalindrome)

euler4 n = maximum . filter isPalindrome $ [x*y | x<-nDigitNums, y<-nDigitNums] where
    nDigitNums = [10^(n-1) .. (10^n)-1]
answer = euler4 3
