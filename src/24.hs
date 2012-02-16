module Main (main,euler24)
where
import Data.List

perm [x] = [[x]]
perm xs = [x:y | x <- xs, y <- perm . delete x $ xs]

euler24 = head . drop 999999 $ perm "0123456789"
main = print euler24
