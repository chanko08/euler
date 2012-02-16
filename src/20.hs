module Main (euler20, main) where
import EulerUtil


euler20 = (sum . intToList . fac) 100
main = print euler20
fac n = product [1..n]
