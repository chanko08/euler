module Main (main,euler22,calcScore) where
import Data.List
import Maybe
import Names

letters = "aABCDEFGHIJKLMNOPQRSTUVWXYZ"

calcScore (rank,name) = rank * (sum  [a | c <- name, let a = (fromJust . elemIndex c) letters ])

euler22 = sum $ map calcScore $ zip [1..] (sort namelist)

main = print euler22
