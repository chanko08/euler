{-
 - Project Euler 39
 -
 - If p is the perimeter of a right angle triangle with integral length sides,
 - {a,b,c}, there are exactly three solutions for p = 120.
 -
 - {20,48,52}, {24,45,51}, {30,40,50}
 - 
 - For which value of p ≤ 1000, is the number of solutions maximised?
 -}
module Main (main, euler39) where
import Data.List
import qualified Data.Set as Set

solns p = [x | x<-triples, sum x == p ]
triples = (Set.toList . Set.fromList) $ [ sort [a,b,c] | n<-[1..99], m<-[n+1..100], k<-[1..50], let a = k*(m^2 - n^2), let b = k * 2 * m * n, let c = k*(m^2 + n^2), a+b+c <= 1000]


euler39 =maximum $ zip soln_list [1000,999..2]
	where
		soln_list = map (length . solns) $ [1000,999..2]
main = print euler39
