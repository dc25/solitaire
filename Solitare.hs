import Haste
import Haste.DOM
import Haste.Events
import Haste.Prim
import Haste.Foreign

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

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

rankSvgStrings = map rankSVGString [Ace .. King]

suitSVGString :: Suit -> String
suitSVGString Hearts =   "heart"
suitSVGString Diamonds = "diamond"
suitSVGString Spades =   "spade"
suitSVGString Clubs =    "club"

suitSvgStrings = map suitSVGString [Hearts .. Clubs]

svgString (Card rank suit) = rankSVGString rank ++ "_" ++ suitSVGString suit

fromSvgString :: String -> Maybe Card 
fromSvgString svg = let rankAndSuit = span (/='_') svg 

                        rankIndex = elemIndex (fst rankAndSuit) rankSvgStrings
                        suitIndex = elemIndex (tail $ snd rankAndSuit) suitSvgStrings

                        -- there must be a better way than nested case
                        in case rankIndex of
                            Nothing -> Nothing
                            Just rank -> case suitIndex of
                                             Nothing -> Nothing
                                             Just suit -> Just $ Card (toEnum rank :: Rank) (toEnum suit :: Suit)

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
        showVisible = (map pc $ zip [length hidden..] (reverse visible))
            where pc (vindex,card) = placeTableCard (svgString card) ("visibleColumn"++show hindex) hindex vindex
    in sequence_ $ showHidden++showVisible

showGame :: Game -> IO ()
showGame game@(Game foundations columns deck reserves)  = 
        let numberedColumns = zip [0..] columns
        in sequence_ $ map showColumn numberedColumns
         
-- align card with id, css class, column, depth in column
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
    let alignVisible = (map pc $ zip [length hidden..] (reverse visible))
            where pc (vindex,card) = alignTableCard (svgString card) ("visibleColumn"++show hindex) hindex vindex
    in sequence_ $ alignVisible

alignGame :: Game -> IO ()
alignGame game@(Game foundations columns deck reserves)  = 
        let numberedColumns = zip [0..] columns
        in sequence_ $ map alignColumn numberedColumns
         
setCallbacks :: Game -> IO ()
setCallbacks game = do
        setDragEndCallback_ffi $ (toPtr $ onDragEnd game)

-- fix later to avoid dealing with [Column], a game internal
columnIndexFromJSCardId :: JSString -> [Column] -> Maybe Int 
columnIndexFromJSCardId jsStr cg = do
           cardId <- fromJSString jsStr -- unwrapping a Maybe
           card <- fromSvgString cardId -- unwrapping a Maybe
           columnIndex card cg -- wrapping a maybe

onDragEnd :: Game -> JSString -> Int -> Int -> IO ()
onDragEnd game@(Game _ cg _ _) jsCardId x y = 
    let draggedToColumn = (y > yColumnPlacement && x > xColumnPlacement)
    in if draggedToColumn then 
           let sourceColumnIndex = columnIndexFromJSCardId jsCardId cg
               destColumnIndex  = min 6 $ (x - xColumnPlacement) `div` xSep
               isValidMove = case sourceColumnIndex 
                           of Nothing -> False
                              Just sourceColumnIndex' -> (last.visible $ cg !! sourceColumnIndex') `goesOnColumn` (cg !! destColumnIndex)
               validSourceColumnIndex = fromMaybe 0 sourceColumnIndex 

           in if isValidMove then do
                   let newGame = fromColumnToColumn game validSourceColumnIndex destColumnIndex
                   alignGame newGame
                   setCallbacks newGame
              else -- not a valid move for some reason
                   alignGame game
       else -- drag destination was not on a column
           alignGame game


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
