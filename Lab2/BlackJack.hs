module BlackJack where
import Cards
import RunGame
import Test.QuickCheck

hand2 = (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))

sizeSteps :: [Integer]
sizeSteps = [ size hand2
            , size (Add (Card (Numeric 2) Hearts)
                        (Add (Card Jack Spades) Empty))
            , 1 + size (Add (Card Jack Spades) Empty)
            , 1 + 1 + size (Empty)
            , 1 + 1 + 0
            , 2]


--display :: Hand -> String
--display = 


displayCard :: Card -> String
displayCard (Card rank suit) = show rank ++ " of " ++  show suit





--displayCard c = show c.Rank ++ "of" ++ show c.Suit       
        