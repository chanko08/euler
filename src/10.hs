module Main (main, euler10) where
import EulerUtil

euler10 = sum $ takeWhile (<2000000) $ primesTME()
main = print euler10
