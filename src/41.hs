module Main (main, euler41) where
import EulerUtil


isPrime x = testPrime' 2
	where
		testPrime' y
			| y >= x `div` 2 = True
			| x `mod` y == 0 = False
			| otherwise = testPrime' (y+1)

--n must be 7, because summing 1 to n for any number n is divisible by 3, hence it can't be prime. 4 works too, but
--its doubtful that that is the max (this is project euler after all)
pandigitals =filter isPrime . map (\x->read x::Int) . perm $ "7654321"

euler41 = take 1 pandigitals 
main = print euler41
