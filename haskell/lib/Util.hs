module Util (perm)
where
import Data.Char
import Data.List

--Show all permutations of a list
perm :: (Eq a) => [a] -> [[a]]
perm [x] = [[x]]
perm xs = [x:y | x <- xs, y <- perm . delete x $ xs]


--Turn an integer into a list of its digits
intToList :: Int -> [Int]
intToList x  = map digitToInt $ show x


--group elements in a list into a x by y matrix
groupByDiag x y list = topSide matrix ++ leftSide matrix ++ topSide revMatrix ++ leftSide revMatrix
	where
		matrix = groupByRow x list
		revMatrix = map reverse matrix
		topSide m = [diag (0,x) (j,y) m | j <- [0..y-1]]
		leftSide m = [diag (i,x) (0,y) m | i <- [1..x-1]]
		diag (i,x) (j,y) matrix
			| i == x || j == y = []
			| otherwise = matrix !! i !! j : (diag (i+1,x) (j+1,y) matrix)

--takes in a list, and groups the elements in a row like sense, where n is the
--number of elements per row
groupByRow _ [] = []	
groupByRow n xs = take n xs : groupByRow n (drop n xs)

--takes in a list, and group the elements in a column like sense, where
-- n is the number of elements per row here too
--TODO change so that n is number of elements per column
groupByCol n xs = transpose $ groupByRow n xs
