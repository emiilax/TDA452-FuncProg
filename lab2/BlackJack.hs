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
--3.1
empty :: Hand
empty = Empty

example_card_1 = Card (Numeric 5) Clubs
example_card_2 = Card (Numeric 5) Diamonds
example_card_3 = Card (Numeric 8) Clubs

example_hand_1 = Add example_card_1 Empty
example_hand_2 = Add example_card_2 example_hand_1
example_hand_3 = Add example_card_3 example_hand_2

--3.4
value :: Hand -> Integer
value Empty = 0
value hand | (nbrOfAces hand) == 1 && value (removeAces hand) <11 = value (removeAces hand) + 11
           | (nbrOfAces hand) == 1 && value (removeAces hand) >=11 = value (removeAces hand) + nbrOfAces hand
           | (nbrOfAces hand) > 1 = value (removeAces hand) + nbrOfAces hand
value (Add card hand) = cardValue card + value hand

cardValue :: Card -> Integer
cardValue (Card r s) = rankValue r

removeAces :: Hand -> Hand
removeAces Empty = Empty
removeAces (Add (Card r s) hand) | r == Ace = removeAces hand
removeAces (Add (Card r s) hand) = (Add (Card r s) (removeAces hand))

rankValue :: Rank -> Integer
rankValue (Numeric m)  = m
rankValue r | r == Ace = 0
rankValue _            = 10

nbrOfAces :: Hand -> Integer
nbrOfAces Empty = 0
nbrOfAces (Add (Card rank suit) hand) | rank == Ace = 1 + (nbrOfAces hand)
nbrOfAces (Add (Card rank suit) hand) = (nbrOfAces hand)

gameOver :: Hand -> Bool
gameOver hand = value hand > 21

winner :: Hand -> Hand -> Player
winner guest bank | value guest <= value bank = Bank
                  | gameOver guest = Bank
                  | gameOver bank = Guest
                  | value guest > value bank = Guest
