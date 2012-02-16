module Main (main, euler31) where

denominations = reverse [1,2,5,10,20,50,100,200]

count _ 0 = 1
count [c] _ = 1
count (c:cs) m = sum  [count cs (m - x) | x <-[0,c..m]]
euler31 = count denominations 200 
main = print euler31
