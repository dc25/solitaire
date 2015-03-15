module Card (
    Card(..),
    Rank(..),
    Suit(..),
    cardColor
) where

import Data.Char

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving  (Eq, Ord, Enum, Bounded, Show, Read)

rankLetter :: Rank -> Char
rankLetter Ace =   'A'
rankLetter Ten =   'T'
rankLetter Jack =  'J'
rankLetter Queen = 'Q'
rankLetter King =  'K'
rankLetter r = chr ((fromEnum r - fromEnum Two) + ord '2')

data Suit = Hearts | Diamonds | Spades | Clubs deriving  (Eq, Ord, Enum, Bounded, Show, Read)

suitLetter :: Suit -> Char
suitLetter Hearts =   'H'
suitLetter Diamonds = 'D'
suitLetter Spades =   'S'
suitLetter Clubs =    'C'

data Color = Red | Black deriving  (Eq, Ord, Enum, Bounded, Show, Read)

suitColor Hearts =   Red
suitColor Diamonds = Red
suitColor Spades =   Black
suitColor Clubs =    Black


data Card = Card {
         rank :: Rank,
         suit :: Suit 
         } 

cardColor (Card _ suit) = suitColor suit


instance Show Card where
    show (Card rank suit) = [rankLetter rank, suitLetter suit]

