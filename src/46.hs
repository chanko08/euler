module Main (main, euler46) where
import EulerUtil
import qualified Data.Set as Set

conject n = (>0) . length . filter (isPrime)  $ sqrs
	where 
		sqrs = takeWhile (>0) . map (\x -> n-2*x*x) $ [1..]

primes = Set.fromList . take 6000 $ primesTME()
isPrime x = x `Set.member` primes

euler46 = head . filter (not . conject) . filter (not . isPrime) $ [9,11..]
main = print euler46
