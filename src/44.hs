module Main (main, euler43) where
import Data.List

pent_nums = map p [1..]
	where p x = (x*(3*x-1)) `div` 2

nperm 1 xs = [[x] | x<-xs]
nperm n xs = [x:y | x<-xs, y<-nperm (n-1) . delete x $ xs]

--make a unique combination of elements, order doesnt matter
ncombo 1 xs = [[x] | x<-xs]
ncombo n xs = [x:y | x<-xs, y <- ncombo (n-1) . filter(>x) $ xs]


is_pent x = (floor(test) == ceiling(test)) && (floor(test) `mod` 6 == 0)
	where test = (sqrt(24*fromIntegral(x)+1) + 1)

euler44 =diff . head . filter (is_pent . diff) . filter (is_pent . sum) . ncombo 2 . take 5000 $ pent_nums
	where
		diff (x:y:[])
			| x <= y = y - x
			| otherwise = x - y
main = print euler43
