{-
 - Project Euler 14
 -
 - The following iterative sequence is defined for the set of positive integers:
 - n → n/2 (n is even)
 - n → 3n + 1 (n is odd)
 -
 - Using the rule above and starting with 13, we generate the following sequence:
 - 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 -
 - It can be seen that this sequence (starting at 13 and finishing at 1)
 - contains 10 terms. Although it has not been proved yet (Collatz Problem),
 - it is thought that all starting numbers finish at 1.
 -
 - Which starting number, under one million, produces the longest chain?
 - 
 - NOTE: Once the chain starts the terms are allowed to go above one million.
 -}
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
