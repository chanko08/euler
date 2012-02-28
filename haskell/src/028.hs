{-
 - Project Euler 28
 -
 - Starting with the number 1 and moving to the right in a clockwise direction
 - a 5x5 spiral is formed as follows:
 -     *21*  22  23  24 *25*
 -      20  *7*  8  *9*  10
 -      19   6  *1*  2   11
 -      18  *5*  4  *3*  12
 -     *17*  16  15  14 *13*
 -
 - It can be verified that the sum of the numbers on the diagonals is 101.
 - What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral
 - formed in the same way?
 -}
module Main (main, euler28) where


diags = 1: diags' 1 1
	where
		diags' n j = (n + 2^j): (n + 2*2^j): (n + 3*2^j): (n + 4*2^j): (diags' (n+4*2^j) (j+1))

lvl n 
	| n == 1 = take 1 diags
	| otherwise = (take 4 . drop (1 + 4*(n-2)))  diags



--returns diagonals of a n*n spiral
diag_square n 
	| even n = []
	| n == 1 = [1]
	| otherwise = n^2: (n^2 - (n-1)): (n^2 - 2*(n-1)): (n^2 - 3*(n-1)): (diag_square (n-2))

euler28 = sum $ diag_square 1001
main = print euler28
