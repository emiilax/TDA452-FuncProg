module BlackJack where
import           Cards
import           RunGame
import           Test.QuickCheck hiding (shuffle)
import           System.Random

-- hand2 = Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty)
--
-- size hand2 = size (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
--            = 1 + size (Add (Card Jack Spades) Empty)
--            = 1 + 1 + Size(Empty)
--            = 1 + 1 + 0
--            = 2
--


implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = shuffle
  }

main :: IO ()
main = runGame implementation

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

--Merges to hands togheter. First hand as input will be at the top of the hand
--and the second will be at the bottom.
(<+) :: Hand -> Hand -> Hand
(<+) Empty hand = hand
(<+) (Add card hand1) hand2 = Add card (hand1 <+ hand2)

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 =
    p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf hand1 hand2 = (size hand1 + size hand2) == size (hand1 <+ hand2)

--function used to get a full deck containing all ranks from all suits
--requires no input
fullDeck :: Hand
fullDeck = fullSuit Clubs <+ fullSuit Diamonds <+ fullSuit Spades <+ fullSuit Hearts

--function used to get all ranks in a suit in one hand. The input suit is what
--suit the user wants the ranks to be in

allRanks = [Numeric n | n<-[2..10]] ++ [Jack, Queen, King, Ace]

fullSuit :: Suit -> Hand
fullSuit suit = fullSuit' allRanks suit

fullSuit' :: [Rank] -> Suit -> Hand
fullSuit' [] _ = Empty
fullSuit' (x:xs) suit = Add (Card x suit) (fullSuit' xs suit)

--fromList :: [Rank] -> Suit -> Hand
--fromList [] _ = Empty
--fromList (x:xs) suit = Add (Card x suit) (fromList xs suit)


-- draws a card from the first hand, and then add it to the other hand. After that
-- the function reurns both the hands
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: The deck is empty."
draw (Add card deck) hand = (deck, Add card hand)

-- function called to play bank. Uses help funtion playBank' to do the accual drawing
playBank :: Hand -> Hand
playBank deck = playBank' deck Empty

-- help function to playBank that does the drawing for the bank. Fills a hand until
-- the decks value is >= 16
playBank' :: Hand -> Hand -> Hand
playBank' deck bankHand | value bankHand < 16 = playBank' deck' bankHand'
  where (deck',bankHand') = draw deck bankHand
playBank' deck bankHand = bankHand

-- function used to shuffle a hand. The input is a StdGen and a Hand. The function
-- creates a new hand and returns that. It puts a random card from the initial Hand,
-- into the top of the new deck. Then recursivly does it on the rest of the hand
-- until its empty. Uses getCard with a random integer to get a random card from
-- the hand and then removes the card from the deck with removeCardFromDeck
shuffle :: StdGen -> Hand -> Hand
shuffle _ Empty = Empty
shuffle g hand = Add rcard (shuffle g1 (removeCardFromDeck rcard hand))
  where (pos, g1) = randomR (0, (size hand-1)) g
        rcard = getCard pos hand

-- returns a card at a given place in a hand. Used in shuffle
getCard :: Integer -> Hand -> Card
getCard n hand | n > size hand = error "getCard: n bigger then hand size"
getCard 0 (Add card hand) = card
getCard n (Add card hand) = getCard(n-1) hand

-- removes a given card from a given hand. After the card is removed, a hand
-- without the card is returned.
removeCardFromDeck :: Card -> Hand -> Hand
removeCardFromDeck c1 Empty = error "removeCardFromDeck: Card not in deck"
removeCardFromDeck c1 (Add c2 hand) | c1 == c2 = hand
removeCardFromDeck c1 (Add c2 hand) = Add c2 (removeCardFromDeck c1 hand)

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
    c `belongsTo` h == c `belongsTo` shuffle g h

belongsTo :: Card -> Hand -> Bool
c `belongsTo` Empty = False
c `belongsTo` (Add c' h) = c == c' || c `belongsTo` h

--Checks if shuffled hand size is as big as unshuffled hand size
prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = size hand == size (shuffle g hand)
