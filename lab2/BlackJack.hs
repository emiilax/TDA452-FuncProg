module BlackJack where
import           Cards
import           RunGame
import           Test.QuickCheck
import           System.Random

-- hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
--
-- size hand2 = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
--            = 1 + size (Add (Card Jack Spades) Empty)
--            = 1 + 1 + Size(Empty)
--            = 1 + 1 + 0
--            = 2
--

example_card_1 = Card King Clubs
example_card_2 = Card Ace Spades
example_card_3 = Card (Numeric 5) Clubs

example_hand_1 = Add example_card_1 Empty
example_hand_2 = Add example_card_2 example_hand_1
example_hand_3 = Add example_card_3 example_hand_2

--empty: Method that returns an empty hand
empty :: Hand
empty = Empty


-- value: calculates the value of a given hand. If the hand consist Aces, the
-- function check whether the hand consist more than two Aces or if the hand
-- without the Aces are more then 10. In that case, Aces are worth 1 otherwise 11
value :: Hand -> Integer
value Empty = 0
value hand | (numberOfAces hand) == 1 && value (removeAces hand) <  11 = value (removeAces hand) + 11
           | (numberOfAces hand) == 1 && value (removeAces hand) >= 11 = value (removeAces hand) + numberOfAces hand
           | (numberOfAces hand) >  1 = value (removeAces hand) + numberOfAces hand
value (Add card hand) = valueCard card + value hand

-- valueCard: returns the value of a card. Uses valueRank to calculate
valueCard :: Card -> Integer
valueCard (Card r s) = valueRank r

-- valueRanks: returns the value of the rank. Returns 0 if Ace, since we add
-- them in method "value" instead
valueRank :: Rank -> Integer
valueRank (Numeric m)  = m
valueRank Ace          = 0
valueRank _            = 10

-- removeAces: returns a hand with no Aces. Used in vale to calculate value of hand
-- without any Aces
removeAces :: Hand -> Hand
removeAces Empty = Empty
removeAces (Add (Card r s) hand) | r == Ace = removeAces hand
removeAces (Add (Card r s) hand) = Add (Card r s) (removeAces hand)

-- numberOfAces: calculates the numberOfAces in a hand. If there is an ace, add 1,
-- otherwise 0 and continue until hand is empty
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card rank suit) hand) | rank == Ace = 1 + numberOfAces hand
numberOfAces (Add (Card rank suit) hand) = numberOfAces hand

-- gameOver: if hand overrites 21, then game is over
gameOver :: Hand -> Bool
gameOver hand = value hand > 21

-- winner: returns whether the guest or bank wins. It checks who wins by the rules
--given in assignment "The winner is the player with the highest score that does
--not exceed 21. If the players end up with the same score, then the bank wins.
--The bank also wins if both players go bust."
winner :: Hand -> Hand -> Player
winner guest bank | gameOver guest = Bank
                  | gameOver bank = Guest
                  | value guest <= value bank = Bank
                  | value guest > value bank = Guest

(<+) :: Hand -> Hand -> Hand
(<+) Empty hand = hand
(<+) (Add card hand1) hand2 = Add card (hand1 <+ hand2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = (size hand1 + size hand2) == size (hand1 <+ hand2)

fullDeck :: Hand
fullDeck = fullSuit Clubs <+ fullSuit Diamonds <+ fullSuit Spades <+ fullSuit Hearts

fullSuit :: Suit -> Hand
fullSuit suit = Add (Card (Numeric 2) suit)
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
                 ))))))))))))

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, Add card hand)

playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand < 16 = playBank' deck' bankHand'
  where (deck',bankHand') = draw deck bankHand
playBank' deck bankHand = bankHand


shuffle :: StdGen -> Hand -> Hand
shuffle _ Empty = Empty
shuffle g hand = Add rcard(shuffle g (newDeck rcard hand))
    where rcard = getCard (fst(randomR (0, (size hand-1)) g)) hand

getCard :: Integer -> Hand -> Card
getCard 0 (Add card hand) = card
getCard n (Add card hand) = getCard(n-1) hand

newDeck :: Card -> Hand -> Hand
newDeck c1 (Add c2 hand) | c1 == c2 = hand
newDeck c1 (Add c2 hand) = Add c2 (newDeck c1 hand)

twoRandomIntegers :: StdGen -> (Integer,Integer)
twoRandomIntegers g = (n1, n2)
  where (n1, g1) = randomR (0, 10) g
        (n2, g2) = randomR (0, 10) g1

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h
