mermodule BlackJack where
import           Cards
import           RunGame
import           Test.QuickCheck

-- hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
--
-- size hand2 = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
--            = 1 + size (Add (Card Jack Spades) Empty)
--            = 1 + 1 + Empty
--            = 1 + 1 + 0
--            = 2
--

example_card_1 = Card King Clubs
example_card_2 = Card Ace Spades
example_card_3 = Card (Numeric 5) Clubs

example_hand_1 = Add example_card_1 Empty
example_hand_2 = Add example_card_2 example_hand_1
example_hand_3 = Add example_card_3 Empty

--empty: Method that returns an empty hand
empty :: Hand
empty = Empty



-- value: calculates the value of a given hand. If the hand consist Aces, the
-- function check whether the hand consist more than two Aces or if the hand
-- without the Aces are more then 10. In that case, Aces are worth 1 otherwise 11
value :: Hand -> Integer
value Empty = 0
value hand | (nbrOfAces hand) == 1 && value (removeAces hand) <  11 = value (removeAces hand) + 11
           | (nbrOfAces hand) == 1 && value (removeAces hand) >= 11 = value (removeAces hand) + nbrOfAces hand
           | (nbrOfAces hand) >  1 = value (removeAces hand) + nbrOfAces hand
value (Add card hand) = cardValue card + value hand

-- cardValue: returns the value of a card. Uses rankValue to calculate
cardValue :: Card -> Integer
cardValue (Card r s) = rankValue r

-- rankValues: returns the value of the rank. Returns 0 if Ace, since we add
-- them in method "value" instead
rankValue :: Rank -> Integer
rankValue (Numeric m)  = m
rankValue Ace          = 0
rankValue _            = 10

-- removeAces: returns a hand with no Aces. Used in vale to calculate value of hand
-- without any Aces
removeAces :: Hand -> Hand
removeAces Empty = Empty
removeAces (Add (Card r s) hand) | r == Ace = removeAces hand
removeAces (Add (Card r s) hand) = (Add (Card r s) (removeAces hand))

-- nbrOfAces: calculates the nbrOfAces in a hand. If there is an ace, add 1,
-- otherwise 0 and continue until hand is empty
nbrOfAces :: Hand -> Integer
nbrOfAces Empty = 0
nbrOfAces (Add (Card rank suit) hand) | rank == Ace = 1 + (nbrOfAces hand)
nbrOfAces (Add (Card rank suit) hand) = (nbrOfAces hand)

-- gameOver: if hand overrites 21, then game is over
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- winner: returns whether the guest or bank wins. It checks who wins by the rules
--given in assignment "The winner is the player with the highest score that does
--not exceed 21. If the players end up with the same score, then the bank wins.
--The bank also wins if both players go bust."
winner :: Hand -> Hand -> Player
winner guest bank | value guest <= value bank = Bank
                  | gameOver guest = Bank
                  | gameOver bank = Guest
                  | value guest > value bank = Guest

(<+) :: Hand -> Hand -> Hand
(<+) Empty hand = hand
(<+) (Add card hand1) hand2 = (Add card (hand1 <+ hand2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = (size hand1 + size hand2) == size (hand1 <+ hand2)

fullDeck :: Hand
fullDeck = fullSuit Clubs <+ fullSuit Diamonds <+ fullSuit Spades <+ fullSuit Hearts

fullSuit :: Suit -> Hand
fullSuit suit = (Add (Card (Numeric 2) suit)
                (Add (Card (Numeric 3) suit)
                (Add (Card (Numeric 4) suit)
                (Add (Card (Numeric 5) suit)
                (Add (Card (Numeric 6) suit)
                (Add (Card (Numeric 7) suit)
                (Add (Card (Numeric 8) suit)
                (Add (Card (Numeric 9) suit)
                (Add (Card (Numeric 10) suit)
                (Add (Card Jack suit)
                (Add (Card Queen suit)
                (Add (Card King suit)
                (Add (Card Ace suit) Empty
                 )))))))))))))

--fullSuit suit = (Add (Card (Numeric a)) suit) | a <- [2..10]

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, (Add card hand))

playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand < 16 = playBank' deck' bankHand'
  where (deck',bankHand') = draw deck bankHand
playBank' deck bankHand = bankHand
