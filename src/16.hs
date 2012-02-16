module Main (main, euler16)
where
euler16 = sum $ map charToInt $ show (2^1000)
	where charToInt x = read [x] :: Int
main = print euler16
