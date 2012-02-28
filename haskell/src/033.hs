{-
 - Project Euler 33
 -
 - The fraction 49/98 is a curious fraction, as an inexperienced mathematician
 - in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which
 - is correct, is obtained by cancelling the 9s.
 -
 - We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
 - 
 - There are exactly four non-trivial examples of this type of fraction, less
 - than one in value, and containing two digits in the numerator and
 - denominator.
 -
 - If the product of these four fractions is given in its lowest common terms,
 - find the value of the denominator.
 -}
module Euler33 (euler33) where
import Data.Ratio

dream a b = a%b == simplify a b where
    simplify a b
        | b `mod` 10 == 0 = 0 % 1
        | a `mod` 10 == b `div` 10 = (a `div` 10) % (b `mod` 10)
        | otherwise = 0 % 1

euler33 = denominator . product $ [a%b | a<-[10..99], b<-[10..99], a<b, dream a b] 
