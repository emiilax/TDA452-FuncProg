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
rankValue Ace | nbrOfAces < 2 || valueMinusAces < 10  = 11
rankValue Ace = 1
rankValue _ = 10

nbrOfAces :: Hand -> Integer
nbrOfAces Empty = 0
nbrOfAces (Add (Card r s) hand) | r == Ace = 1 + nbrOfAces hand
nbrOfAces (Add (Card r s) hand) = nbrOfAces hand

valueMinusAces :: Hand -> Integer
valueMinusAces Empty = 0
valueMinusAces (Add (Card r s) hand) | r == Ace = valueMinusAces hand
valueMinusAces (Add (Card r s) hand) = rankValue r + valueMinusAces hand

value :: Hand -> Integer
value Empty = 0
--value (Add (Card r s) hand) | r == Ace = value hand
value (Add (Card r s) hand) = rankValue r + value hand

--Hand hand1 = (Add (Card Ace Spades) Empty)
--gameOver :: hand -> Hand -> Player

--winner :: Hand -> Hand -> Player
