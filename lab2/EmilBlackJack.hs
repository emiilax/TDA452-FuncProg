module BlackJack where
import Cards
import RunGame
import Test.QuickCheck
import System.Random

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
value hand = (valueMinusAces hand) + nbrOfAces hand * 11

(<+) :: Hand -> Hand -> Hand
Empty <+ hand2 = hand2
hand1 <+ Empty = hand1
(Add card hand1) <+ (hand2) = Add card (hand1 <+ hand2)

listOfRank :: [Rank]
listOfRank = [(Numeric 2),
              (Numeric 3),
              (Numeric 4),
              (Numeric 5),
              (Numeric 6),
              (Numeric 7),
              (Numeric 8),
              Jack,
              Queen,
              King,
              Ace]

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

draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, (Add card hand))



playBank :: Hand -> Hand
playBank hand = playBank' hand Empty

playBank' :: Hand -> Hand -> Hand
playBank' deck bankhand | value bankhand >= 16 = bankhand
playBank' deck bankhand = playBank' deck' bankhand'
  where (deck', bankhand') = draw deck bankhand



--getSuitCards :: Suit -> Hand
--getSuitCards s = Add (Card s r) Empty
--    where r =

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3


prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = (size hand1 + size hand2) == size (hand1 <+ hand2)

shuffle1 :: StdGen -> Hand -> Hand
shuffle1 _ Empty = Empty
shuffle1 g hand = Add rcard (shuffle1 g (newDeck rcard hand))
    where rcard = getCard (fst(randomR (0, (size hand-1)) g)) hand




newDeck :: Card -> Hand -> Hand
newDeck c1 (Add c2 hand) | c1 == c2 = hand
newDeck c1 (Add c2 hand) = Add c2 (newDeck c1 hand)

getCard :: Integer -> Hand -> Card
getCard 0 (Add card hand) = card
getCard n (Add card hand) = getCard (n-1) hand


twoRandomIntegers :: StdGen -> (Integer,Integer)
twoRandomIntegers g = (n1, n2)
  where (n1, g1) = randomR (0, 10) g
        (n2, g2) = randomR (0, 10) g1


fullD = fullDeck


hand1 = Add (Card Ace Spades) (Add (Card Ace Spades) (Add (Card (Numeric 3) Spades) Empty))
hand2 = (Add (Card (Numeric 5) Spades) Empty)

example_card_1 = Card King Clubs
example_card_2 = Card Ace Spades
example_card_3 = Card (Numeric 5) Clubs

example_hand_1 = Add example_card_1 Empty
example_hand_2 = Add example_card_2 example_hand_1

example_hand_3 = Add example_card_3 Empty
--Hand hand1 = (Add (Card Ace Spades) Empty

--gameOver :: Hand -> Bool



--winner :: Hand -> Hand -> Player
