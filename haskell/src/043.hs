{-
 - Project Euler 43
 -
 - The number, 1406357289, is a 0 to 9 pandigital number because it is made up
 - of each of the digits 0 to 9 in some order, but it also has a rather
 - interesting sub-string divisibility property.
 -
 - Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way,
 - we note the following:
 -
 -   d2d3d4=406 is divisible by 2
 -   d3d4d5=063 is divisible by 3
 -   d4d5d6=635 is divisible by 5
 -   d5d6d7=357 is divisible by 7
 -   d6d7d8=572 is divisible by 11
 -   d7d8d9=728 is divisible by 13
 -   d8d9d10=289 is divisible by 17
 -   
 - Find the sum of all 0 to 9 pandigital numbers with this property.
 -}
module Euler43 (euler43) where
import Data.List

divides y x = x `rem` y == 0

listToInt xs =sum . map (\(x,y)->y*10^x) . zip [0..] $ reverse xs

nperm 1 xs = [[x] | x<-xs]
nperm n xs = [x:y | x <-xs ,y <- nperm (n-1) . delete x $ xs ]

digits = [0..9]

soln = map listToInt $ soln' [17,13,11,7,5,3,2] (nperm 3 digits) where
    soln' [] ls = ls
    soln' (x:xs) ls
        | x == 17 = soln' xs (filterDiv ls)
        | otherwise = soln' xs (filterDiv newls) where
            newls = [x:y | x <-digits, y<-ls, x `notElem` y]
            filterDiv = filter (divides x . listToInt . take 3)


euler43 = sum soln
answer = euler43
