{-
 - Project Euler 54
 -
 - In the card game poker, a hand consists of five cards and are ranked,
 - from lowest to highest, in the following way:
 -
 - High Card: Highest value card.
 - One Pair: Two cards of the same value.
 - Two Pairs: Two different pairs.
 - Three of a Kind: Three cards of the same value.
 - Straight: All cards are consecutive values.
 - Flush: All cards of the same suit.
 - Full House: Three of a kind and a pair.
 - Four of a Kind: Four cards of the same value.
 - Straight Flush: All cards are consecutive values of same suit.
 - Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
 -
 - The cards are valued in the order:
 - 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
 -
 - If two players have the same ranked hands then the rank made up of the
 - highest value wins; for example, a pair of eights beats a pair of fives
 - (see example 1 below). But if two ranks tie, for example, both players have
 - a pair of queens, then highest cards in each hand are compared (see example
 - 4 below); if the highest cards tie then the next highest cards are compared,
 - and so on.
 -
 - Consider the following five hands dealt to two players:
 - Hand	 	Player 1	 	Player 2	 	 Winner
 - 1	 	5H 5C 6S 7S KD	2C 3S 8S 8D TD   Player 2
 -          Pair of Fives   Pair of Eights
 - 
 - 2 	    5D 8C 9S JS AC	2C 5C 7D 8S QH	 Player 1
 -          High Card Ace   High Card Queen
 -
 - 3	 	2D 9C AS AH AC	3D 6D 7D TD QD   Player 2
 -          Three Aces      Diamond Flush
 - 
 - 4	 	4D 6S 9H QH QC	3D 6D 7H QD QS   Player 1
 -          Queen Pair      Queen Pair
 -          High Card Nine  High Card Seven
 -
 - 5	 	2H 2D 4C 4D 4S  3C 3D 3S 9S 9D   Player 1
 -          Full House w/   Full House w/
 -          3 Fours         3 Threes
 -
 - The file, poker.txt, contains one-thousand random hands dealt to two players.
 - Each line of the file contains ten cards (separated by a single space): the
 - first five are Player 1's cards and the last five are Player 2's cards. You
 - can assume that all hands are valid (no invalid characters or repeated
 - cards), each player's hand is in no specific order, and in each hand there is
 - a clear winner.
 -
 - How many hands does Player 1 win?
 -}
module Euler54 (euler54)
where
import Data.Maybe (fromJust)
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

createHands s = map formHands rounds where
    rounds = map (splitAt 5 . words) . lines $ s
    parseHand =  map parseCard
    parseCard c = (parse allRanks . head $ c, parse allSuits . last $ c)
    parse a r = fromJust . flip elemIndex a $ r
    allRanks = "23456789TJQKA"
    allSuits = "SCHD"
    formHands (x,y) = (parseHand x, parseHand y)

firstPlayerWon (p1,p2) = determineHand p1 > determineHand p2

euler54 h = length . filter firstPlayerWon $ h

answer = do 
    s <- readFile "poker.txt"
    let hands = createHands s
        result = euler54 hands
    print result
