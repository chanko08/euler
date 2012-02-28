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
module Euler39 (euler39) where
import Data.List
import Data.Set(toList, fromList)

solns p = [x | x<-toList . fromList $ triples, sum x == p ]
triples = [ sort [a,b,c] | n<-[1..99], m<-[n+1..100], k<-[1..50], let a = f k m n, let b = g k m n, let c = h k m n, a+b+c <= 1000]
    where
        f k m n = k*(m^2 - n^2)
        g k m n = 2*k*m*n
        h k m n = k*(m^2 + n^2)
    


euler39 =maximum $ zip solnList [1000,999..2] where
    solnList = map (length . solns) [1000,999..2]

answer = euler39
