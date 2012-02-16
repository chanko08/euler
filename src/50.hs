module Main(euler50, main) where
import Numbers
import Data.List

primes = takeWhile (<1000000) $ primesTME

findSeq nums p = filter (not . null) . map (\x-> findSeq' x [] 0) . tails $ nums where
	findSeq' [] ys n
		| n /= p = []
		| otherwise = ys
	findSeq' (x:xs) ys n
		| x + n < p = findSeq' xs (x:ys) (x + n)
		| x + n == p = (x:ys)
		| otherwise = []

findSeqCount nums p = maximum . map (\x -> findSeqCount' x 0 0) . tails . takeWhile (<p) $ nums where
	findSeqCount' [] c n
		| n /= p = 0
		| otherwise = c + 1
	findSeqCount' (x:xs) c n
		| x + n < p = findSeqCount' xs (c+1) (x+n)
		| x + n == p = c + 1
		| otherwise = 0

sortBySeq (p,pc) (q,qc) 
	| pc > qc = GT
	| pc < qc = LT
	| pc == qc = compare p q
		
euler50 = maximumBy sortBySeq . map (\x -> (x, findSeqCount primes x)) $ primes 
main = print euler50