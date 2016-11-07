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

example_card_1 = Card (Numeric 5) Clubs
example_card_2 = Card Jack Spades
example_card_3 = Card Ace Spades

example_hand_1 = Add example_card_1 Empty
example_hand_2 = Add example_card_2 example_hand_1
example_hand_3 = Add example_card_3 example_hand_2

value :: Hand -> Integer
value Empty = 0
value (Add (Card rank suit) hand) = cardValue rank + value hand

cardValue :: Rank -> Integer
cardValue (Numeric m)  = m
cardValue r | r == Ace = 0
cardValue _            = 10

nbrOfAces :: Hand -> Integer
nbrOfAces Empty = 0
nbrOfAces (Add (Card rank suit) Empty) | rank == Ace = 1
nbrOfAces (Add (Card rank suit) hand) | rank == Ace = 1 + (nbrOfAces hand)

--gameOver :: Hand -> Hand -> Player

--winner :: Hand -> Hand -> Player
