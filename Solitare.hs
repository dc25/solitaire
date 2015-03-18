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
foreign import ccall showAlert :: JSString -> IO ()
foreign import ccall setDragEndCallback :: Ptr (JSString -> Int -> Int -> IO ()) -> IO ()

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
         
setCallbacks :: Game -> IO ()
setCallbacks game = do
        setDragEndCallback $ (toPtr $ onDragEnd game)

onDragEnd :: Game -> JSString -> Int -> Int -> IO ()
onDragEnd game string x y = 
        let string' = fromJSStr string ++ " " ++ show x ++ " " ++ show y
        in showAlert $ toJSStr string'

loadCallback = do
    shuffledDeck <- shuffle [ Card r s | r<-[Ace .. King], s<-[Hearts .. Clubs]] 
    let
        foundations' = replicate 4 []
        columns' =     replicate 7 $ Column [] []
        deck' =       []
        game = Game foundations' columns' deck' shuffledDeck
        gameInPlay = start game
    showGame $ gameInPlay
    setCallbacks gameInPlay

main = do 
          loadCards(toPtr loadCallback)
