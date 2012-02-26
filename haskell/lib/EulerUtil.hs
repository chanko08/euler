module EulerUtil (fibs, primesTME, isPalindrome, groupConsecDigits, intToList, groupByDiag, groupByCol, groupByRow, factors, triangleNums, numDivisors, choose, perm, primeFactors) where
import Data.Char
import Data.List


perm [x] = [[x]]
perm xs = [x:y | x <- xs, y <- perm . delete x $ xs]

choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k


fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

isPalindrome x = (map digitToInt $ show x) == (reverse $ map digitToInt $ show x)

groupConsecDigits l (x:xs)
	| length (x:xs) >= l = (take l (x:xs)) : groupConsecDigits l xs
	| otherwise = []

intToList x  = map digitToInt $ show x

-- x and y are matrix side lengths 
groupByDiag x y list = topSide matrix ++ leftSide matrix ++ topSide revMatrix ++ leftSide revMatrix
	where
		matrix = groupByRow x list
		revMatrix = map reverse matrix
		topSide m = [diag (0,x) (j,y) m | j <- [0..y-1]]
		leftSide m = [diag (i,x) (0,y) m | i <- [1..x-1]]
		diag (i,x) (j,y) matrix
			| i == x || j == y = []
			| otherwise = matrix !! i !! j : (diag (i+1,x) (j+1,y) matrix)

-- n is the number of elements per row
groupByRow _ [] = []	
groupByRow n xs = take n xs : groupByRow n (drop n xs)

-- n is the number of elements per row here too
--TODO change so that n is number of elements per column
groupByCol n xs = transpose $ groupByRow n xs



primeFactors 1 = []
primeFactors n = primeFactor : primeFactors (n `div` primeFactor)
	where
		primeFactor = head $ filter evenly $ primesTME()
		evenly i = n `mod` i == 0

numDivisors n = product $ map ((1+) . length) $ group $ primeFactors n


factors n = 1 : n : (factor' n 2)
	where
		factor' n i
			| i >= limit = []
			| n `mod` i == 0 = i : (n `div` i) : (factor' n (i+1))
			| otherwise = factor' n (i+1)
		limit = floor(sqrt(fromInteger n)) + 1
		
		



triangleNums = 1: zipWith (+) [2..] triangleNums

--setup for the sieve of eratosthenes
myminus (x:xs) (y:ys) = case (compare x y) of
	LT -> x: myminus xs (y:ys)
	EQ -> myminus xs ys
	GT -> myminus (x:xs) ys
myminus xs _ = xs

myunion (x:xs) (y:ys) = case (compare x y) of
	LT -> x : myunion xs (y:ys)
	EQ -> x : myunion xs ys
	GT -> y : myunion (x:xs) ys
myunion xs ys = xs ++ ys
  
primesToQ m = 2 : sieve [3,5..m] where
	sieve [] = []
	sieve (p:xs) = p : sieve (xs `myminus` [p*p, p*p + 2*p .. m]) 

primesTME () = 2: ([3, 5 ..] `myminus` join [[p*p, p*p + 2*p ..] | p <- primes'])
	where 
		primes' = 3 : ([5, 7 ..] `myminus` join [[p*p, p*p+2*p ..] | p <- primes'])
		join ((x:xs):t) = x : myunion xs (join (pairs t))
		pairs ((x:xs):ys:t) = (x : myunion xs ys) : pairs t 

