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
module Euler57 (euler57) where
import Util(intToList)

compute x y = x + 2*y  
nums = 1 : 3 : zipWith compute nums (tail nums)
denoms = 1 : 2 : zipWith compute denoms (tail denoms)

f (x,y) = (length . intToList) x > (length . intToList) y
euler57 = length . filter f . take 1000 $ zip nums denoms
answer = euler57
