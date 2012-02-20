{-
 - Project Euler 57
 -
 - It is possible to show that the square root of two can be expressed as an
 - infinite continued fraction.
 -
 - √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
 -
 - By expanding this for the first four iterations, we get:
 -
 - 1 + 1/2 = 3/2 = 1.5
 - 1 + 1/(2 + 1/2) = 7/5 = 1.4
 - 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
 - 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
 -
 - The next three expansions are 99/70, 239/169, and 577/408, but the eighth
 - expansion, 1393/985, is the first example where the number of digits in the
 - numerator exceeds the number of digits in the denominator.
 -
 - In the first one-thousand expansions, how many fractions contain a numerator
 - with more digits than denominator?
 -}
module Main (main, euler57) where


num 0 = 1
num 1 = 3
num x = 2 * num (x - 1) + num (x - 2)


denom 0 = 1
denom 1 = 2
denom x = 2 * denom (x - 1) + denom (x -2)

euler57 = length . filter (\(x,y) -> x > y) . map (\x -> (num x, denom x) )  $ [1..1000]
main = print euler57
