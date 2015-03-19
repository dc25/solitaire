module Card (
    Card(..),
    Rank(..),
    Suit(..),
    cardColor
) where

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving  (Eq, Ord, Enum, Bounded, Show, Read)

data Suit = Hearts | Diamonds | Spades | Clubs deriving  (Eq, Ord, Enum, Bounded, Show, Read)

data Color = Red | Black deriving  (Eq, Ord, Enum, Bounded, Show, Read)

suitColor:: Suit -> Color
suitColor Hearts =   Red
suitColor Diamonds = Red
suitColor Spades =   Black
suitColor Clubs =    Black

data Card = Card {
         rank :: Rank,
         suit :: Suit 
         } deriving  (Eq)

cardColor:: Card -> Color
cardColor (Card _ suit') = suitColor suit'

