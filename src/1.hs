module Main (main, euler1) where
euler1 = sum [x | x <- [1 .. 999], (x `mod` 3 == 0) || (x `mod` 5 == 0)]

main = print euler1
