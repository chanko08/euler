{-
 - Project Euler 33
 -
 - The fraction 49/98 is a curious fraction, as an inexperienced mathematician
 - in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which
 - is correct, is obtained by cancelling the 9s.
 -
 - We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
 - 
 - There are exactly four non-trivial examples of this type of fraction, less
 - than one in value, and containing two digits in the numerator and
 - denominator.
 -
 - If the product of these four fractions is given in its lowest common terms,
 - find the value of the denominator.
 -}
module Main (main, euler33) where

dumb_simplify a b
	| a `mod` 10 == b `div` 10 = (a `div` 10, b `mod` 10)
	| otherwise = (a, b)

dream :: (Integral a) => a -> a -> Bool
dream a b = y/z == w/x && w /= y && x /=z && y/z  /= 1
	where
		(y,z) = (fromIntegral a, fromIntegral b)
		(w,x) = (fromIntegral . fst $ ans, fromIntegral . snd $ ans)
		ans = dumb_simplify a b 
		


euler33 = ((fst f) `div` g, (snd f) `div` g)
	where 
		f = foldl (\x y-> (fst x * fst y, snd x * snd y )) (1,1) [(a,b) | a <- [10..99], b <- [10..99], dream a b]
		g = gcd (fst f) (snd f)
 
main = print euler33
