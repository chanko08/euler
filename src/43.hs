module Main (main, euler43) where
import EulerUtil
import Data.List

divides y x = x `rem` y == 0

listToInt xs =sum . map (\(x,y)->y*10^x) . zip [0..] $ reverse xs

nperm 1 xs = [[x] | x<-xs]
nperm n xs = [x:y | x <-xs ,y <- nperm (n-1) . delete x $ xs ]

digits = [0..9]

soln = map listToInt $ soln' [17,13,11,7,5,3,2] (nperm 3 digits)
	where
		soln' [] ls = ls
		soln' (x:xs) ls
			| x == 17 = soln' xs (filterDiv $ ls)
			| otherwise = soln' xs (filterDiv $ newls)
			where
				newls = [x:y | x <-digits, y<-ls, not $ elem x y]
				filterDiv = filter (divides x . listToInt . take 3)


euler43 = sum soln
main = print euler43
