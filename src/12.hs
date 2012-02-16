module Main (main, euler12, test) where
import EulerUtil

euler12 = head $ filter (\x -> numDivisors x > 500) triangleNums
test = head $ filter (\x -> (numDivisors x) > 5) triangleNums
main = print euler12


