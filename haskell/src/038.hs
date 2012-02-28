{-
 - Project Euler 39
 -
 - Take the number 192 and multiply it by each of 1, 2, and 3:
 -
 -   192 × 1 = 192
 -   192 × 2 = 384
 -   192 × 3 = 576
 - 
 - By concatenating each product we get the 1 to 9 pandigital, 192384576.
 - We will call 192384576 the concatenated product of 192 and (1,2,3)
 -
 - The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, 
 - and 5, giving the pandigital, 918273645, which is the concatenated product of
 - 9 and (1,2,3,4,5).
 -
 - What is the largest 1 to 9 pandigital 9-digit number that can be formed as 
 - the concatenated product of an integer with (1,2, ... , n) where n > 1?
 -}
module Euler38 (euler38) where
import Data.List

isPandigital n = sort n == "123456789"

concatProd n = concatProd' prods "" where
    prods = map (\x -> show (x*n)) [1..]
    concatProd' (p:ps) rs
        | length rs >= 9 = rs
        | otherwise = concatProd' ps (rs ++ p)

euler38 = maximum . map (\x-> read x::Int  ) .filter isPandigital . map concatProd $ [2..10000]
answer = euler38
