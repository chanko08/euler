module Main (euler23,main,isAbundant) where
import Data.List
import qualified Data.Set as Set
import EulerUtil


isAbundant n = (sum $ (init . sort . factors) n) > n

euler23 = sum $ filter (`Set.notMember` abundant_sums)  [1..28123]
	where 
		abundants = filter isAbundant [1..28123]
		abundant_sums = Set.fromList [ c | a<-abundants, b<-abundants, a < b, let c = (a+b), c < 28123]
		
main = print euler23
