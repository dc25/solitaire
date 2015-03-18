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

foreign import ccall loadCards_ffi :: Ptr (IO ()) -> IO ()
foreign import ccall placeCard_ffi :: JSString -> JSString -> Int -> Int -> IO ()
foreign import ccall alignCard_ffi :: JSString -> JSString -> Int -> Int -> IO ()
foreign import ccall showAlert_ffi :: JSString -> IO ()
foreign import ccall setDragEndCallback_ffi :: Ptr (JSString -> Int -> Int -> IO ()) -> IO ()

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

-- place card with id, css class, column, depth in column
placeTableCard :: String -> String -> Int -> Int -> IO ()
placeTableCard id cssClass columnIndex positionInColumn =
        placeCard_ffi (toJSStr id) 
                  (toJSStr cssClass)
                  (xColumnPlacement+ xSep*columnIndex) 
                  (yColumnPlacement+ ySep*positionInColumn)

-- assign vindex indicating depth in column to each card in column
-- display card in column position specified by (hindex,vindex)
showColumn :: (Int, Column) -> IO ()
showColumn (hindex, (Column hidden visible)) = 
    let showHidden = (map ph $ zip [0..] hidden)
            where ph (vindex,_) = placeTableCard "back" ("hiddenColumn"++show hindex) hindex vindex
        showVisible = (map pc $ zip [length hidden..] visible)
            where pc (vindex,card) = placeTableCard (svgString card) ("visibleColumn"++show hindex) hindex vindex
    in sequence_ $ showHidden++showVisible

showGame :: Game -> IO ()
showGame game@(Game foundations columns deck reserves)  = 
        let numberedColumns = zip [0..] columns
        in sequence_ $ map showColumn numberedColumns
         
-- place card with id, css class, column, depth in column
alignTableCard :: String -> String -> Int -> Int -> IO ()
alignTableCard id cssClass columnIndex positionInColumn =
        alignCard_ffi (toJSStr id) 
                  (toJSStr cssClass)
                  (xColumnPlacement+ xSep*columnIndex) 
                  (yColumnPlacement+ ySep*positionInColumn)

-- assign vindex indicating depth in column to each card in column
-- display card in column position specified by (hindex,vindex)
alignColumn :: (Int, Column) -> IO ()
alignColumn (hindex, (Column hidden visible)) = 
    let alignVisible = (map pc $ zip [length hidden..] visible)
            where pc (vindex,card) = alignTableCard (svgString card) ("visibleColumn"++show hindex) hindex vindex
    in sequence_ $ alignVisible

alignGame :: Game -> IO ()
alignGame game@(Game foundations columns deck reserves)  = 
        let numberedColumns = zip [0..] columns
        in sequence_ $ map alignColumn numberedColumns
         
setCallbacks :: Game -> IO ()
setCallbacks game = do
        setDragEndCallback_ffi $ (toPtr $ onDragEnd game)

onDragEnd :: Game -> JSString -> Int -> Int -> IO ()
onDragEnd game string x y = 
        let string' = fromJSStr string ++ " " ++ show x ++ " " ++ show y
        in showAlert_ffi $ toJSStr string'

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
          loadCards_ffi(toPtr loadCallback)
