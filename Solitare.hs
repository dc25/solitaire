import Haste
import Haste.DOM
import Haste.Events
import Haste.Prim
import Haste.Foreign

import Data.Char
import Control.Monad
import Shuffle
import Card
import Game

foreign import ccall loadCards :: Ptr (IO ()) -> IO ()
foreign import ccall placeCard :: JSString -> Int -> Int -> IO ()

rankSVGString :: Rank -> String
rankSVGString Ten =   "10"
rankSVGString Jack =  "jack"
rankSVGString Queen = "queen"
rankSVGString King =  "king"
rankSVGString r = [chr ((fromEnum r - fromEnum Ace) + ord '1')]

suitSVGString :: Suit -> String
suitSVGString Hearts =   "heart"
suitSVGString Diamonds = "diamond"
suitSVGString Spades =   "spade"
suitSVGString Clubs =    "club"

svgString (Card rank suit) = rankSVGString rank ++ "_" ++ suitSVGString suit

showDeck :: [Card] -> IO ()
showDeck deck = 
           sequence_ (map pc $ zip [ (x,y) | x <- [0..70], y <- [0..4] ] deck)
               where pc ((x,y), card) = placeCard (toJSStr $ svgString card) (100*x) (30*y)

showGame :: Game -> IO ()
showGame game = return ()

loadCallback = do
    shuffledDeck <- shuffle [ Card r s | r<-[Ace .. King], s<-[Hearts .. Clubs]] 
    let
        foundations' = replicate 4 []
        columns' =     replicate 7 $ Column [] []
        deck' =       []
        game = Game foundations' columns' deck' shuffledDeck
        gameInPlay = start game
    showDeck $ shuffledDeck
    return () -- Why is this necessary?  Should this be necessary?

main = do 
          loadCards(toPtr loadCallback)
