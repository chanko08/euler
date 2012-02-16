module Main (main, euler5) where

euler5 = foldl (lcm) 1 [1..20]
main = print euler5
