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

loadCallback = do
    shuffledDeck <- shuffle [ Card r s | r<-[Ace .. King], s<-[Hearts .. Clubs]] 
    let
        foundations' = replicate 4 []
        columns' =     replicate 7 $ Column [] []
        deck' =       []
        game = Game foundations' columns' deck' shuffledDeck
        gameInPlay = start game

    placeCard (toJSStr "1_club") 0 0 
    placeCard (toJSStr "king_diamond") 400 50
    placeCard (toJSStr "3_heart") 300 100
    placeCard (toJSStr "black_joker") 200 150
    placeCard (toJSStr "jack_spade") 340 250

main = do 
          loadCards(toPtr loadCallback)
