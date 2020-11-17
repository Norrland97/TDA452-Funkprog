module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

-- These hands are used for testruns
hand2 = (Add (Card (Numeric 2) Hearts)
                       (Add (Card Jack Spades) Empty))  -- No aces, hand value 13
hand3 = (Add (Card (Numeric 2) Hearts)
                       (Add (Card Jack Spades) 
                                      (Add (Card Ace Spades) Empty))) -- One ace which has value 1, hand value 14
hand4 = (Add (Card (Numeric 3) Spades) 
                       (Add (Card Ace Spades) Empty))         --One ace which has value 11, hand value 14                           

--shows the steps taken by the 'size' function
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size (Empty)
            , 1 + 1 + 0
            , 2]

-- converts the hand to a string, cards seperated with a blank space.
display :: Hand -> String
display Empty = ""
display (Add card hand) = displayCard card ++ " " ++ display hand

-- converts a card to a string
displayCard :: Card -> String
displayCard (Card (Numeric n) suit) = show n ++ " of " ++  show suit  
displayCard (Card rank suit)        = show rank ++ " of " ++  show suit
 
-- calculates the value of a hand, checs if it's above 21 if so recalculates with aces as value 1 
value :: Hand -> Integer
value Empty = 0
value hand = valueCount highAce hand
        where highAce = valueCount True hand <= 21

-- recursively checks the value of a hand. Argument boolean sets the value for the aces of the hand.
valueCount :: Bool -> Hand -> Integer
valueCount bool Empty                       = 0
valueCount bool (Add (Card rank suit) hand) = (valueRank bool rank) + (valueCount bool hand)

-- checks the value of a card. Argument boolean sets ace value depending on if it gets a true or false. 
valueRank :: Bool -> Rank -> Integer
valueRank _ Jack       = 10
valueRank _ Queen      = 10
valueRank _ King       = 10
valueRank True Ace     = initialValue
        where initialValue = 11
valueRank False Ace    = newAce
        where newAce = 1
valueRank _ rank       = numToInt rank
        where numToInt (Numeric i) = i


-- checks whether a hand passes 21 in value
gameOver :: Hand -> Bool
gameOver hand = value hand > 21 
                   
-- returns the winner of argument two hands, where the first is the guest player and the second the bank player
winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest               = Bank
                  | gameOver bank                = Guest
                  | (value guest) > (value bank) = Guest
                  | otherwise                    = Bank

