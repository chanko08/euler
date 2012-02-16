module Main (main, euler6) where

euler6 = (sum [x | x<-[1..100]])^2 - (sum [x^2 | x<-[1..100] ])
main = print euler6
