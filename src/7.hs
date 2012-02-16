module Main (main, euler7) where
import EulerUtil

euler7 = last $ take 10001 $ primesTME()
main = print euler7
