module Main (main, euler3) where
import EulerUtil

euler3 = head $ filter (\x -> 600851475143 `mod` x ==0) $ reverse $ takeWhile( <floor(sqrt(600851475143)) ) $ primesTME()
main = print euler3
