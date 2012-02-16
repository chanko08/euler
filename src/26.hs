module Main (main, euler26) where
import Data.List
import Maybe
--decimal expansion of y/x where y < x
decExp y x = decExp' (10*y) x []
	where
		decExp' y x ps
			| r `elem` ps = [d]
			| otherwise = d : (decExp' (r*10) x (r:ps))
			where
				(d,r) = y `divMod` x



period y x = (init . snd . splitAt e) $ expansion
	where
		expansion = decExp y x
		e = (fromJust . elemIndex (last expansion)) $ expansion
 
euler26 = snd . maximum $ [(l,x) | x<-[2..1000], let l = length $ period 1 x] 
main = print euler26 
