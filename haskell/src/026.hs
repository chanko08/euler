{-
 - Project Euler 26
 -
 - A unit fraction contains 1 in the numerator. The decimal representation of 
 - the unit fractions with denominators 2 to 10 are given:
 -
 -
 -      1/2  = 0.5
 -      1/3  = 0.(3)
 -      1/4  = 0.25
 -      1/5  = 0.2
 -      1/6  = 0.1(6)
 -      1/7  = 0.(142857)
 -      1/8  = 0.125
 -      1/9  = 0.(1)
 -      1/10 = 0.1
 -
 - Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
 - seen that 1/7 has a 6-digit recurring cycle.
 - 
 - Find the value of d < 1000 for which 1/d contains the longest recurring cycle
 - in its decimal fraction part.
 -}
module Euler26 (euler26) where
import Data.List
import Data.Maybe
--decimal expansion of y/x where y < x
decExp y x = decExp' (10*y) x [] where
    decExp' y x ps
        | r `elem` ps = [d]
        | otherwise = d : decExp' (r*10) x (r:ps) where
            (d,r) = y `divMod` x



period y x = init . snd . splitAt e $ expansion where
    expansion = decExp y x
    e = fromJust . elemIndex (last expansion) $ expansion
 
euler26 n = snd . maximum $ [(l,x) | x<-[2..n], let l = length $ period 1 x]
answer = euler26 1000
