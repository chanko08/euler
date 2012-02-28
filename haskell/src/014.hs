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
 - it is thought that all starting numbers fish at 1.
 -
 - Which starting number, under one million, produces the longest chain?
 - 
 - NOTE: Once the chain starts the terms are allowed to go above one million.
 -}
module Euler14 (euler14)
where
import Data.List

{-
	NOTE: for this to be at any decent speed, add the optimization flag -O3
	to the compilation using ghc, otherwise this will just be balls slow.
-}

collatz 1 = [1]
collatz n 
    | even n    = n : collatz (n `div` 2)
    | otherwise = n : collatz (3 * n + 1)

euler14 = fst . maximum . zip (map (length . collatz) [10..1000000]) $ [10..1000000]
answer = euler14

