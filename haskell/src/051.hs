{-
 - Project Euler 51
 -
 - By replacing the 1st digit of *3, it turns out that six of the nine possible
 - values: 13, 23, 43, 53, and 83, are all prime.
 -
 - By replacing the 3rd and 4th digits of 56**3 with the same digit, this
 - 5-digit number is the first example having seven primes among the ten
 - generated numbers, yielding the family:
 -
 -  56003, 56113, 56333, 56443, 56663, 56773, 56993
 -
 -  Consequently, 56003, being the first member of this family, is the
 -  smallest prime with this property.
 -
 -  Find the smallest prime which, by replacing part of the number (not
 -  necessarily adjacent digits) with the same digit, is part of an eight
 -  prime value family
 -}
module Main (main, euler51) where
import qualified Data.Set as Set
import Data.List(tails)

import Numbers(primesTME)
import Util(perm)

primes = takeWhile (<1000000) $ primesTME

masks = Set.toList . Set.unions $ [make_masks x | x <-[5..6]]

make_masks x = Set.unions [make_masks' i | i <-[3..x-1]] where
    make_masks' i = Set.fromList . perm . concat $ [ [1 | j <-[1..i]], [0 | j <- [1..(x-i)]] ]

mask_match m p
    | (length . show $ p) /= length m = False
    | otherwise = all_eq $ map snd . filter (\(x,y) -> x == 1 ) $ zip m (show p)

all_eq (p:ps) = all (==p) ps where


families m ps =filter (\x -> (length x) > 7) . map check_equal . tails $ ps where
    check_equal [] = []
    check_equal (p:ps) = p :( filter (mask_eq m p) $ ps)

mask_eq m p q = all (\(x,y) -> x == y) . map snd . filter (\(x,y) -> x == 0) . zip m $ zip (show p) (show q)

euler51 = head . concat . map (\(m, ps) -> families m ps) . zip masks . map (\x -> filter (mask_match x) primes) $ masks
main = print euler51
