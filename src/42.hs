module Main (main, euler42) where
import Data.Char
import Words

charToInt [] = []
charToInt (c:cs) = (alphaIndex . toUpper) c: charToInt cs
	where
		alphaIndex c = fromIntegral $ ord c - 64 

isTriangleNum x = (floor sqr) == (ceiling sqr)
	where
		sqr = sqrt $ 8*x + 1

euler42 = length . filter isTriangleNum . map (sum . charToInt) $ wordlist
main = print euler42
