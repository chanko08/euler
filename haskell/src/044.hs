{-
 - Project Euler 44
 -
 - Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2. The first ten
 - pentagonal numbers are:
 -
 -      1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
 -
 - It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference,
 - 70 − 22 = 48, is not pentagonal.
 -
 - Find the pair of pentagonal numbers, Pj and Pk, for which their sum and
 - difference is pentagonal and D = |Pk − Pj| is minimised;
 -
 - what is the value of D?
 -}
module Euler44 (euler44) where
import Data.List

pentNums = map p [1..]
	where p x = (x*(3*x-1)) `div` 2

nperm 1 xs = [[x] | x<-xs]
nperm n xs = [x:y | x<-xs, y<-nperm (n-1) . delete x $ xs]

--make a unique combination of elements, order doesnt matter
ncombo 1 xs = [[x] | x<-xs]
ncombo n xs = [x:y | x<-xs, y <- ncombo (n-1) . filter(>x) $ xs]


isPent x = (floor test == ceiling test) && (floor test `mod` 6 == 0)
	where test = sqrt(24*fromIntegral x + 1) + 1

euler44 = diff . head . filter (isPent . diff) . filter (isPent . sum) . ncombo 2 . take 5000 $ pentNums
    where diff (x:y:[])
            | x <= y = y - x
            | otherwise = x - y