module Main (main, euler45) where

triNums = map tri [1..]
	where tri n = n*(n+1) `div` 2

isHex n = ceiling(t) == floor(t)
	where t = (sqrt(8*fromIntegral(n) +1) + 1)/4

isPent n = floor(t) == ceiling(t)
	where t = (sqrt(24*fromIntegral(n) + 1) +1) / 6 

euler45 = head . filter isPent . filter isHex . drop 285 $ triNums 
main = print euler45
