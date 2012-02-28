{-
 - Project Euler 16
 -
 - 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 - What is the sum of the digits of the number 2^1000?
 -}
module Euler16 (euler16)
where
import Util(intToList)
euler16 n = sum . intToList $ n
answer = euler16 (2^1000)
