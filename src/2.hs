module Main (main, euler2) where
import EulerUtil
euler2 = sum $ filter (\x -> x `mod` 2 ==0) $ takeWhile (<4000000) fibs
main = print euler2
