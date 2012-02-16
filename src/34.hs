{-
 - Project Euler 34
 -
 - 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
 - Find the sum of all numbers which are equal to the sum of the factorial
 - of their digits.
 -
 - Note: as 1! = 1 and 2! = 2 are not sums they are not included.
 -}
module Main (main, euler34) where
import EulerUtil

fac n = product [1..n]
faccish a =  ((a==) . sum . map fac .intToList) a 


euler34 = sum [a |  a <- [3..50000], faccish a]
main = print euler34
