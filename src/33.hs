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
