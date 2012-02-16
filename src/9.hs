module Main (main, euler9) where

euler9 = foldl (*) 1 $ head [[a,b,c] | a<-[1..1000], b<-[1..1000], let c = 1000-a-b, a<b, a^2+b^2==c^2]
main = print euler9
