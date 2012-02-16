module Main(euler49, main) where
import Numbers
import Data.List(sort)

primes = takeWhile (\x -> x < 10000) . dropWhile (\x -> x < 1000) $ primesTME
candidates = [(a,b) | a <-primes, b <- dropWhile (<=a) primes, sort (show a) == sort (show b)]
formatAnswer (a,b,c) = show a ++ show b ++ show c

euler49 = formatAnswer . head . drop 1 $ [(a,b,c) | (a,b)<-candidates, let c = 2*b - a, sort (show c) == sort (show a), c `elem` primes]
main = print euler49