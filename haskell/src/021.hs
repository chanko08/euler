{-
 - Project Euler 21
 -
 - Let d(n) be defined as the sum of proper divisors of n
 - (numbers less than n which divide evenly into n). If d(a) = b and d(b) = a,
 - where a ≠ b, then a and b are an amicable pair and each of a and b are
 - called amicable numbers.
 - 
 - For example, the proper divisors of 220 are
 - 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
 -
 - therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142;
 - so d(284) = 220.
 -
 - Evaluate the sum of all the amicable numbers under 10000.
 -}
module Euler21 (euler21) where
import Data.List
import qualified Data.Set as Set
import Numbers(factors)

d = sum . init . sort . factors

euler21 = sum . map (uncurry (+)) . Set.toList . Set.fromList $ amicables where
    amicables = [(max a b, min a b) | a <- [1..10000], let b = d a, d b == a, a /= b]

answers = euler21
