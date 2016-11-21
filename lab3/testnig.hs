data Suit = Spades | Hearts | Diamonds | Clubs
            deriving (Eq,Enum,Bounded)

instance Show Suit where
  show Spades   = "♠"
  show Hearts   = "♥"
  show Diamonds = "♦"
  show Clubs    = "♣"


data Test = (Maybe Int)

test::Test
test = Nothing

instance Show Test where
    show n = show n
