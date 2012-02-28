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
module Euler15 (euler15)
where
import Numbers(choose)

-- Yay combinatorics!
euler15 n = (2*n) `choose` n 
answer = euler15 20
