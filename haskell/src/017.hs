{-
 - Project Euler 17
 -
 - If the numbers 1 to 5 are written out in words: one, two, three, four, five,
 - then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 -
 - If all the numbers from 1 to 1000 (one thousand) inclusive were written out
 - in words, how many letters would be used?
 -
 - NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
 - forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
 - letters. The use of "and" when writing out numbers is in compliance with
 - British usage.
 -}
module Euler17 (euler17) where
import Util(intToList)
import Numbers(divides)


ones = ["","one","two","three","four","five","six","seven","eight","nine"]

specialNums = ["ten","eleven","twelve","thirteen","fourteen","fifteen",
    "sixteen","seventeen","eighteen","nineteen"]

tens = ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty",
    "ninety"]

stringifyDigit x
    | x == 1000            = "onethousand"
	| 0 == x               = "" 
	| 1 <= x && x < 10     = ones !! x
	| 10 <= x && x < 20    = specialNums !! last digits
	| 20 <= x && x < 100   = tens !! head digits ++ ones !! last digits
	| divides 100 x        = ones !! head digits ++ "hundred"
	| 100 <= x && x < 1000 = ones !! head digits ++ "hundredand" ++ stringifyDigit (x `mod` 100)
	where digits = intToList x

euler17 n
    | n <= 1000 = sum . map (length . stringifyDigit) $ [1..n]

answer = euler17 1000

