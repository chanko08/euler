module Main (main, euler34) where
import EulerUtil

fac n = product [1..n]
faccish a =  ((a==) . sum . map fac .intToList) a 


euler34 = sum [a |  a <- [3..50000], faccish a]
main = print euler34
