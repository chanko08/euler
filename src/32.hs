module Main (main, euler32) where
import Data.List
import qualified Data.Set as Set
isPandigital (x,y) = sort (show x ++ show y ++ show (x*y)) == "123456789"


euler32 = (sum . Set.toList . Set.fromList) [x*y | x<-[1..50], y<-[1..2000], isPandigital (x,y)]
main = print euler32
