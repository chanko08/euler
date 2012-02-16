module Main (main, euler38) where
import Data.List

isPandigital n = (sort n) == "123456789"

concatProd n = concatProd' prods ""
	where
		prods = map (\x -> show (x*n))  $ [1..]
		concatProd' (p:ps) rs
			| (length (rs)) >= 9 = rs
			| otherwise = (concatProd' ps (rs ++ p))

euler38 = maximum . map (\x-> read x::Int  ) .filter isPandigital . map concatProd $ [2..10000]
main = print euler38
