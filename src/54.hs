module Main (main)
where
import Maybe (fromJust)
import Data.List (elemIndex, sort, group,nub)

determineHand h = (handRank h, tiebreak) where
    tiebreak = [kind x h | x <- [4,3,2,1]]

handRank h 
    | straight h && flush h      = 9
    | hasKind 4 h                = 8
    | hasKind 2 h && hasKind 3 h = 7
    | flush h                    = 6
    | straight h                 = 5
    | hasKind 3 h                = 4
    | length (kind 2 h) == 2     = 3
    | hasKind 2 h                = 2
    | otherwise                  = 1

--determine if a hand has a straight. This excludes ace low straights
straight hand = ((==5) . length $ kind 1 hand) && 4 == head r - last r where
    r = ranks hand

--determine if a flush exists in the hand
flush hand = (==1) . length . nub . map snd $ hand

--find what pairs a had has. returns the ranks that have a group of x
kind x hand = map head . filter ((x==) . length) . group . ranks $ hand
hasKind x hand = not . null . kind x $ hand

ranks hand = reverse . sort . map fst $ hand
suits hand = sort . map snd $ hand

createHands s = map (\(x,y) -> (parseHand x, parseHand y)) $ rounds where
    rounds = map (splitAt 5 . words) . lines $ s
    parseHand cs =  map parseCard $ cs
    parseCard c = (parse allRanks . head $ c, parse allSuits . last $ c)
    parse a r = fromJust . (flip elemIndex) a $ r
    allRanks = "23456789TJQKA"
    allSuits = "SCHD"

firstPlayerWon (p1,p2) = determineHand p1 > determineHand p2

euler54 h = length . filter firstPlayerWon $ h

main = do 
    s <- readFile "poker.txt"
    let hands = createHands s
        result = euler54 hands
    print result
