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

xSep = 100
ySep = 30
xColumnPlacement = 200
yColumnPlacement = 100

showDeck :: [Card] -> IO ()
showDeck deck = 
        sequence_ (map pc $ zip [ (x,y) | x <- [0..70], y <- [0..4] ] deck)
            where pc ((x,y), card) = placeCard (toJSStr $ svgString card) (xSep*x) (ySep*y)

showColumn :: (Int, Column) -> IO ()
showColumn (hindex, (Column hidden visible)) = 
        let showHidden = (map ph $ zip [0..] hidden)
                where ph (vindex,_) = placeCard (toJSStr $ "back") (xColumnPlacement+ xSep*hindex) (yColumnPlacement+ ySep*vindex)
            showVisible = (map pc $ zip [length hidden..] visible)
                where pc (vindex,card) = placeCard (toJSStr $ svgString card) (xColumnPlacement+ xSep*hindex) (yColumnPlacement+ ySep*vindex)
        in sequence_ $ showHidden++showVisible

showGame :: Game -> IO ()
showGame game@(Game foundations columns deck reserves)  = 
        let numberedColumns = zip [0..] columns
        in sequence_ $ map showColumn numberedColumns
         

loadCallback = do
    shuffledDeck <- shuffle [ Card r s | r<-[Ace .. King], s<-[Hearts .. Clubs]] 
    let
        foundations' = replicate 4 []
        columns' =     replicate 7 $ Column [] []
        deck' =       []
        game = Game foundations' columns' deck' shuffledDeck
        gameInPlay = start game
    -- showDeck $ shuffledDeck
    showGame $ gameInPlay
    return () -- Without this nothing displays. Why is this necessary?  

main = do 
          loadCards(toPtr loadCallback)
