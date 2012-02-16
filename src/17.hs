module Main (main, euler17) where

ones = ["","one","two","three","four","five","six","seven","eight","nine"]

special_nums = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen",
	"eighteen","nineteen"]

tens = ["","","twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

intToDigits 0 = []
intToDigits x = intToDigits (x `div` 10) ++ [x `mod` 10]

stringifyDigit x
	| 0 == x = "" 
	| 1 <= x && x < 10 = ones !! x
	| 10 <= x && x < 20 = special_nums !! (last digits)
	| 20 <= x && x < 100 = tens !! (head digits) ++ ones !! (last digits)
	| x `mod` 100 == 0 = ones !! (head digits) ++ "hundred"
	| 100 <= x && x < 1000 = ones !! (head digits) ++ "hundredand" ++ stringifyDigit (x `mod` 100)
	| otherwise = "fuck"

	where
		digits = intToDigits x

euler17 = (sum . map length) nums
	where
		nums = "onethousand": ( map stringifyDigit $ [1..999])

main = print euler17
