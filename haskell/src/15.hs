{-
 - Project Euler 15
 -
 - Starting in the top left corner of a 2×2 grid, there are 6 routes
 - (without backtracking) to the bottom right corner.
 - +-->+-->+   +-->+---+   +-->+---+
 - |   |   |   |   |   |   |   |   |
 - +---+---v   +---v-->+   +---v---+
 - |   |   |   |   |   |   |   |   |
 - +---+---v   +---+---v   +---v-->+
 -
 - +---+---+   +---+---+   +---+---+
 - |   |   |   |   |   |   |   |   |
 - v-->+-->+   v-->+---+   v---+---+
 - |   |   |   |   |   |   |   |   |
 - +---+---v   +---v-->+   v-->+-->+
 -
 - How many routes are there through a 20×20 grid?
 -}
module Main (main, euler15)
where
import EulerUtil

-- Yay combinatorics!
euler15 = 40 `choose` 20 
main = print euler15
