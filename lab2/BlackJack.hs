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

value :: Hand -> Integer
value Empty = 0
value 



--gameOver :: hand -> Hand -> Player

--winner :: Hand -> Hand -> Player
