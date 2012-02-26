{-
 - Project Euler 52
 -
 - It can be seen that the number, 125874, and its double, 251748, contain 
 - exactly the same digits, but in a different order.
 - 
 - Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x,
 - contain the same digits.
 -}
module Main (main, euler52) where
import Data.List(sort)

all_eq [] = False
all_eq (p:ps) = all (==p) ps

euler52 = fst . head . filter (\(x,y) -> all_eq y) . zip [1..] . map (\x -> [(sort . show) (x*y) | y <-[1..6]]) $ [1..]
main = print euler52
