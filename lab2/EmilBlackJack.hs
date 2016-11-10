module BlackJack where
import           Cards
import           RunGame

-- hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
--
-- size hand2 = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
--            = 1 + size (Add (Card Jack Spades) Empty)
--            = 1 + 1 + Empty
--            = 1 + 1 + 0
--            = 2
--

empty :: Hand
empty = Empty

rankValue :: Rank -> Integer
rankValue (Numeric k) = k
rankValue Ace = 0
rankValue _ = 10

nbrOfAces :: Hand -> Integer
nbrOfAces Empty = 0
nbrOfAces (Add (Card r s) hand) | r == Ace = 1 + nbrOfAces hand
nbrOfAces (Add (Card r s) hand) = nbrOfAces hand

valueMinusAces :: Hand -> Integer
valueMinusAces Empty = 0
valueMinusAces (Add (Card r s) hand) = rankValue r + valueMinusAces hand

value :: Hand -> Integer
value hand | (valueMinusAces hand > 10) || (nbrOfAces hand > 1) = valueMinusAces hand + nbrOfAces hand
value hand = (valueMinusAces hand) + nbrOfAces * 11



hand1 = Add (Card Ace Spades) (Add (Card Ace Spades) (Add (Card (Numeric 3) Spades) Empty))

--Hand hand1 = (Add (Card Ace Spades) Empty

--gameOver :: Hand -> Bool



--winner :: Hand -> Hand -> Player
