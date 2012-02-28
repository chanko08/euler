{-
 - Project Euler 20
 -
 - n! means n × (n − 1) × ... × 3 × 2 × 1
 -
 - For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
 - and the sum of the digits in the number 10! is
 - 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
 -
 - Find the sum of the digits in the number 100!
 -}
module Main (euler20, main) where
import EulerUtil


euler20 = (sum . intToList . fac) 100
main = print euler20
fac n = product [1..n]
