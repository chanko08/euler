module Main (main, euler40) where
import EulerUtil

digitsList = foldr (\x y -> x++y) [] . map intToList $ [1..]

euler40 = product . map (\x -> digitsList !! x) $ [0,9,99,999,9999,99999,999999] 
main = print euler40
