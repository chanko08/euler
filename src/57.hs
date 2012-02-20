{-
 - Project Euler 57
 -}
module Main (main, euler57) where
import EulerUtil(intToList)

compute x y = x + 2*y  
nums = 1 : 3 : zipWith compute nums (tail nums)
denoms = 1 : 2 : zipWith compute denoms (tail denoms)

f (x,y) = (length . intToList) x > (length . intToList) y
euler57 = length . filter f . take 1000 $ zip nums denoms

main = print euler57
