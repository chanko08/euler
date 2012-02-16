module Main (main, euler30) where
import EulerUtil

sumPow p num = sum [a^p | a <-(intToList num)] 

euler30 = sum [a | a <-[1000..200000],  sumPow 5 a == a]
main = print euler30
