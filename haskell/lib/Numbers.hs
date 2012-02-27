module Numbers(
    primesTME, primesToQ, fibs, triangleNums, choose, primeFactors,
    numDivisors, factors, groupConsecDigits
)
where
import Data.Char(digitToInt)
import Data.List(group)

--Groups the a numbers together in a list of xs
groupConsecDigits _ [] = []
groupConsecDigits a xs = take a xs : groupConsecDigits a ys where
    ys = drop a xs


--Test function for if a number is a palindromic number
isPalindrome :: Int -> Bool
isPalindrome x = y == reverse y where
    y = show $ x
        


--Triangle Numbers
triangleNums :: [Int]
triangleNums = 1: zipWith (+) [2..] triangleNums


--Fibonacci Sequence
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)


--simple choose function
choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k


--Functions related to prime numbers and divisibility
--gets the prime factors of a number
primeFactors 1 = []
primeFactors n = p : primeFactors (n `div` p) where
    p = head $ filter evenly $ primesTME
    evenly i = n `mod` i == 0

--returns the number of divisors
numDivisors n = product $ map ((1+) . length) $ group $ primeFactors n

--lists the numbers that divide into a number
factors n = 1 : n : (factor' n 2) where
    factor' n i
        | i >= limit = []
        | n `mod` i == 0 = i : (n `div` i) : (factor' n (i+1))
        | otherwise = factor' n (i+1)
    limit = floor(sqrt(fromInteger n)) + 1


--Prime Sieves  
primesToQ :: Int -> [Int]
primesToQ m = 2 : sieve [3,5..m] where
	sieve [] = []
	sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p + 2*p .. m]) 


primesTME :: [Int]
primesTME = 2: ([3, 5 ..] `minus` join [[p*p, p*p + 2*p ..] | p <- primes'])
	where 
		primes' = 3 : ([5, 7 ..] `minus` join [[p*p, p*p+2*p ..] | p <- primes'])
		join ((x:xs):t) = x : union xs (join (pairs t))
		pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t 

--Prime sieve helper functions
--takes all the numbers of one list and removes them from the list of the other
--returning the numbers that are left
minus :: [Int] -> [Int] -> [Int]
minus (x:xs) (y:ys) = case (compare x y) of
	LT -> x: minus xs (y:ys)
	EQ -> minus xs ys
	GT -> minus (x:xs) ys
minus xs _ = xs


--classic union
union :: [Int] -> [Int] -> [Int]
union (x:xs) (y:ys) = case (compare x y) of
	LT -> x : union xs (y:ys)
	EQ -> x : union xs ys
	GT -> y : union (x:xs) ys
union xs ys = xs ++ ys
