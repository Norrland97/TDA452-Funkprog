module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

-- These hands are used for testruns
hand2 = Add (Card (Numeric 2) Hearts)
                       (Add (Card Jack Spades) Empty)  -- No aces, hand value 13
hand3 = Add (Card (Numeric 2) Hearts)
                       (Add (Card Jack Spades) 
                                      (Add (Card Ace Spades) Empty)) -- One ace which has value 1, hand value 14
hand4 = Add (Card (Numeric 3) Spades) 
                       (Add (Card Ace Spades) Empty)         --One ace which has value 11, hand value 14                           

-------------A0-------------
-- shows the steps taken by the 'size' function
sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size Empty
            , 1 + 1 + 0
            , 2]

-------------A1-------------
-- converts the hand to a string, cards seperated with a blank space.
display :: Hand -> String
display Empty = ""
display (Add card hand) = displayCard card ++ ", " ++ display hand

-- converts a card to a string
displayCard :: Card -> String
displayCard (Card (Numeric n) suit) = show n ++ " of " ++  show suit  
displayCard (Card rank suit)        = show rank ++ " of " ++  show suit
 
-------------A2-------------
-- calculates the value of a hand, checs if it's above 21 if so recalculates with aces as value 1 
value :: Hand -> Integer
value Empty = 0
value hand = valueCount highAce hand
        where highAce = valueCount True hand <= 21

-- recursively checks the value of a hand. Argument boolean sets the value for the aces of the hand.
valueCount :: Bool -> Hand -> Integer
valueCount bool Empty                       = 0
valueCount bool (Add (Card rank suit) hand) = valueRank bool rank + valueCount bool hand

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


-------------A3-------------
-- checks whether a hand passes 21 in value
gameOver :: Hand -> Bool
gameOver hand = value hand > 21 
 
-------------A4-------------                  
-- returns the winner of argument two hands, where the first is the guest player and the second the bank player
winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest               = Bank
                  | gameOver bank                = Guest
                  | value guest > value bank     = Guest
                  | otherwise                    = Bank


-------------B1-------------
-- last card of h1 have h2 as 'hand' in the pattern (Add card hand)
(<+) :: Hand -> Hand -> Hand
(<+) Empty h2            = h2
(<+) (Add card Empty) h2 = Add card h2 
(<+) (Add card hand) h2  = Add card (hand <+ h2)

-------------B2-------------
-- generates and represents a full deck of 52 cards
fullDeck :: Hand 
fullDeck = cardListToHand deckAsList
        where deckAsList = [Card rank suit | rank <- ranks, suit <- suits] 
              ranks      = Ace:King:Queen:Jack:[Numeric x | x <- [2..10]]
              suits      = [Spades,Hearts,Diamonds,Clubs]

-- converts a list of cards to a hand of cards
cardListToHand :: [Card] -> Hand
cardListToHand [] = Empty
cardListToHand cs = Add (head cs) (cardListToHand (tail cs))

-------------B3-------------
-- function moves top card from first argument hand to second argument hand
draw :: Hand -> Hand -> (Hand, Hand) --Deck fist, Hand second
draw Empty _ = error "draw: The deck is empty :( "
draw (Add card deck) hand = (deck, hand <+ Add card Empty)

-------------B4-------------
-- function simulates playing session of a bank and finally returns the banks final Hand
--          deck    hand
playBank :: Hand -> Hand
playBank deck = playBankHelper deck Empty

playBankHelper :: Hand -> Hand -> Hand
playBankHelper deck hand   | value hand < 16 = uncurry playBankHelper postDraw
                           | otherwise       = hand
        where postDraw = draw deck hand
              
-------------B5--------------
-- function shuffles provided Hand
-- function runs removeSelectI with rendomly generated Integer and input Hand 52 times
-- run w: shuffleDeck (mkStdGen <any int you can think of>) fullDeck              
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck _ Empty     = Empty   
shuffleDeck stdGen deck = shuffleDeckHelper stdGen Empty deck
        
-- function with more arguments than shuffleDeck enables recursive transitioning from old to new deck
shuffleDeckHelper :: StdGen -> Hand -> Hand -> Hand
shuffleDeckHelper _ newDeck (Add card Empty)    = Add card Empty <+ newDeck
shuffleDeckHelper g newDeck oldDeck             = shuffleDeckHelper g' (Add (fst removedTuple) newDeck) (snd removedTuple)
        where rnd             = randomR (0, size oldDeck - 1) g
              g'              = snd rnd
              removedTuple    = removeSelectI (fst rnd) oldDeck 

-- function removes Card on specific index and returns tuple with the card and the hand without the card
removeSelectI :: Integer -> Hand -> (Card, Hand)
removeSelectI 0 (Add card hand)   = (card, hand)
removeSelectI int (Add card hand) = (fst recurve, Add card (snd recurve))
        where recurve = removeSelectI (int -1) hand


-----------B6-----------
implementation = Interface
  { iFullDeck = fullDeck
  , iValue    = value
  , iDisplay  = display
  , iGameOver = gameOver
  , iWinner   = winner 
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffleDeck
  }

main :: IO ()
main = runGame implementation

-------------Test Funftions-------------
-- provided testing functions
prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1 + size h2) == size (h1 <+ h2)

--function checks if hand changes size
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffleDeck g hand)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffleDeck g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty      = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
