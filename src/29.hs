module Main (main, euler29) where
import qualified Data.Set as Set


euler29 = (Set.size . Set.fromList) $ [a^b | a <- [2..100], b <- [2..100]]
main = print euler29
