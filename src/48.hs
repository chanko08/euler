module Main(main,euler48) where

euler48 = reverse . take 10 . reverse . show . sum $ [x^x | x<-[1..1000]]
main = print euler48