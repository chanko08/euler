{-
 - Project Euler 4
 -
 - A palindromic number reads the same both ways. The largest palindrome made 
 - from the product of two 2-digit numbers is 9009 = 91 × 99. 
 -
 - Find the largest palindrome made from the product of two 3-digit numbers.
 -}
module Main (main, euler4) where
import EulerUtil

euler4 = maximum $ filter (isPalindrome) $ [x*y | x<-[100..999], y<-[100..999]]
main = print euler4
