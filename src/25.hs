module Main (main,euler25) where
import EulerUtil

euler25 = fst . last . takeWhile (\x -> (length . intToList . snd $ x) /= 1000) . zip [1..] $ fibs
main = print euler25
