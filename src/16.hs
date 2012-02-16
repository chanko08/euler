{-
 - Project Euler 16
 -
 - 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 - What is the sum of the digits of the number 2^1000?
 -}
module Main (main, euler16)
where
euler16 = sum $ map charToInt $ show (2^1000)
	where charToInt x = read [x] :: Int
main = print euler16
