module Main (main, euler4) where
import EulerUtil

euler4 = maximum $ filter (isPalindrome) $ [x*y | x<-[100..999], y<-[100..999]]
main = print euler4
