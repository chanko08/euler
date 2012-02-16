module Main (main, euler14, collatzNum)
where
import Data.List

{-
	NOTE: for this to be at any decent speed, add the optimization flag -O3
	to the compilation using ghc, otherwise this will just be balls slow.
-}

collatzNum 0 = 1
collatzNum 1 = 1
collatzNum n 
	| even n = collatzNum (n `div` 2) + 1
	| otherwise = collatzNum ((3*n+1) `div` 2) + 2

euler14 = fst $ maximumBy tupMax lens
	where
		lens = zip [500001,500003..1000000] (map collatzNum [500001,500003..1000000])
		tupMax (a,b) (c,d) = compare b d

main = print euler14
