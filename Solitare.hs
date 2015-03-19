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

-- javascript functionality
foreign import ccall loadCards_ffi :: Ptr (IO ()) -> IO ()
foreign import ccall placeCard_ffi :: JSString -> JSString -> Int -> Int -> IO ()
foreign import ccall alignCard_ffi :: JSString -> JSString -> Int -> Int -> IO ()
foreign import ccall deleteBySelectionString_ffi :: JSString -> IO ()
foreign import ccall showAlert_ffi :: JSString -> IO ()
foreign import ccall setMouseoverCallback_ffi :: Ptr (JSString -> Int -> Int -> IO ()) -> IO ()
foreign import ccall setDragEndCallback_ffi :: Ptr (JSString -> JSString -> Int -> Int -> IO ()) -> IO ()

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

-- conversion from Card to javascript id
svgString (Card rank suit) = rankSVGString rank ++ "_" ++ suitSVGString suit

-- conversion from javascript id to Card
fromSvgString :: String -> Maybe Card 
fromSvgString svg = 
    let rankAndSuit = span (/='_') svg 

        rankIndex = elemIndex (fst rankAndSuit) rankSvgStrings
        suitIndex = elemIndex (tail $ snd rankAndSuit) suitSvgStrings

        -- there must be a better way than nested case
        in case rankIndex of
            Nothing -> Nothing
            Just rank -> case suitIndex of
                             Nothing -> Nothing
                             Just suit -> Just $ Card (toEnum rank :: Rank) (toEnum suit :: Suit)

-- Parameters controlling game layout 
xSep = 90
ySep = 20
xColumnPlacement = 40
yColumnPlacement = 160

xFoundationPlacement = xColumnPlacement + 3*xSep
yFoundationPlacement = 20

xReservesPlacement = xColumnPlacement + xSep `div` 2
yReservesPlacement = yFoundationPlacement

xDeckPlacement = xReservesPlacement + xSep 
yDeckPlacement = yFoundationPlacement

deleteHiddenColumn :: Int -> IO ()
deleteHiddenColumn hindex = 
    deleteBySelectionString_ffi $ toJSStr (".hiddenColumn"++show hindex)

deleteVisibleColumn :: Int -> IO ()
deleteVisibleColumn hindex = 
    deleteBySelectionString_ffi $ toJSStr (".visibleColumn"++show hindex)

deleteColumn :: Int -> IO ()
deleteColumn hindex = do
    deleteHiddenColumn hindex 
    deleteVisibleColumn hindex 


-- place card with id, css class, column, depth in column
placeTableCard :: String -> String -> Int -> Int -> IO ()
placeTableCard id cssClass columnIndex positionInColumn =
        placeCard_ffi (toJSStr id) 
                  (toJSStr cssClass)
                  (xColumnPlacement+ xSep*columnIndex) 
                  (yColumnPlacement+ ySep*positionInColumn)

-- place card with id, column on foundation
placeFoundationCard :: String -> String -> Int -> IO ()
placeFoundationCard id cssClass hindex =
        let vindex = 0
        in placeCard_ffi (toJSStr id) 
                  (toJSStr cssClass)
                  (xFoundationPlacement+ xSep*hindex) 
                  (yFoundationPlacement+ ySep*vindex)

-- place card with id, class, on deck
placeDeckCard :: String -> String -> IO ()
placeDeckCard id cssClass =
        let vindex = 0
            hindex = 0
        in placeCard_ffi (toJSStr id) 
                  (toJSStr cssClass)
                  (xDeckPlacement+ xSep*hindex) 
                  (yDeckPlacement+ ySep*vindex)

-- place card with id, class, on reserves
placeReservesCard :: String -> String -> IO ()
placeReservesCard id cssClass =
        let vindex = 0
            hindex = 0
        in placeCard_ffi (toJSStr id) 
                  (toJSStr cssClass)
                  (xReservesPlacement+ xSep*hindex) 
                  (yReservesPlacement+ ySep*vindex)

-- display blanks to indicate where cards go if column is empty.
showEmptyColumn :: Int -> IO ()
showEmptyColumn hindex = 
    placeTableCard "base_only" ("emptyColumn"++show hindex) hindex 0

-- assign vindex indicating depth in column to each hidden card in column
-- display back of card in column position specified by (hindex,vindex)
showHiddenColumn :: Int -> Column -> IO ()
showHiddenColumn hindex (Column hidden _) = 
    sequence_ (map ph $ zip [0..] hidden)
            where ph (vindex,_) = placeTableCard "back" ("hiddenColumn"++show hindex) hindex vindex

-- assign vindex indicating depth in column to each visible card in column
-- display card in column position specified by (hindex,vindex)
showVisibleColumn :: Int -> Column -> IO ()
showVisibleColumn hindex (Column hidden visible) = 
    sequence_ (map pc $ zip [length hidden..] (reverse visible))
            where pc (vindex,card) = placeTableCard (svgString card) ("visibleColumn"++show hindex) hindex vindex

-- assign vindex indicating depth in column to each card in column
-- display card in column position specified by (hindex,vindex)
showColumn :: Int -> Column -> IO ()
showColumn hindex column = do
    showEmptyColumn hindex 
    showHiddenColumn hindex column
    showVisibleColumn hindex column

showFoundation :: Int -> [Card] -> IO ()
showFoundation hindex foundation = do
    placeFoundationCard "base_only" ("emptyFoundation"++show hindex) hindex 

showDeck :: [Card] -> IO ()
showDeck deck = do
    placeDeckCard "base_only" "emptyDeck" 

showReserves :: [Card] -> IO ()
showReserves deck = do
    placeReservesCard "base_only" "emptyReserves" 
    sequence_ $ map (\_ -> placeReservesCard "back" "hiddenReserves") deck

showGame :: Game -> IO ()
showGame game@(Game foundations columns deck reserves)  =  do
    sequence_ $ map (uncurry showColumn) (zip [0..] columns)
    sequence_ $ map (uncurry showFoundation) (zip [0..] foundations)
    showDeck deck
    showReserves reserves
         
-- align card with id, css class, column, depth in column
alignTableCard :: String -> String -> Int -> Int -> IO ()
alignTableCard id cssClass columnIndex positionInColumn =
        alignCard_ffi (toJSStr id) 
                  (toJSStr cssClass)
                  (xColumnPlacement+ xSep*columnIndex) 
                  (yColumnPlacement+ ySep*positionInColumn)

-- assign vindex indicating depth in column to each card in column
-- display card in column position specified by (hindex,vindex)
alignColumn :: Int -> Column -> IO ()
alignColumn hindex (Column hidden visible) = 
    sequence_ (map pc $ zip [length hidden..] (reverse visible))
            where pc (vindex,card) = alignTableCard (svgString card) ("visibleColumn"++show hindex) hindex vindex

-- align card in foundation with id, css class, column
alignFoundationCard :: String -> String -> Int -> IO ()
alignFoundationCard id cssClass foundationIndex =
    let positionInFoundation = 0 
    in alignCard_ffi (toJSStr id) 
              (toJSStr cssClass)
              (xFoundationPlacement+ xSep*foundationIndex) 
              (yFoundationPlacement+ ySep*positionInFoundation)

-- display card in foundation position specified by hindex
alignFoundation:: Int -> [Card] -> IO ()
alignFoundation hindex foundation = 
    sequence_ (map pc $ (reverse foundation))
            where pc card = alignFoundationCard (svgString card) ("foundation"++show hindex) hindex 

alignGame :: Game -> IO ()
alignGame game@(Game foundations columns deck reserves)  = do
        sequence_ $ map (uncurry alignColumn) $ zip [0..] columns
        sequence_ $ map (uncurry alignFoundation) $ zip [0..] foundations
         
setCallbacks :: Game -> Maybe String -> IO ()
setCallbacks game topClass = do
        setDragEndCallback_ffi $ (toPtr $ onDragEnd game topClass)
        setMouseoverCallback_ffi $ (toPtr $ onMouseover game topClass)

columnIndexFromJSCardId :: JSString -> Game -> Maybe Int 
columnIndexFromJSCardId jsStr game = do
    cardId <- fromJSString jsStr -- unwrapping a Maybe
    card <- fromSvgString cardId -- unwrapping a Maybe
    columnIndex card game -- wrapping a maybe

onMouseover :: Game -> Maybe String -> JSString -> Int -> Int -> IO ()
onMouseover game@(Game _ cg _ _) topClass jsCardId x y =
    let sourceColumnIndex = columnIndexFromJSCardId jsCardId game
    in case sourceColumnIndex of
           Nothing -> return ()
           Just sourceColumnIndex' -> do
               let newTopClass = (".visibleColumn" ++ show sourceColumnIndex')
               if (Just newTopClass /= topClass) then do
                   deleteBySelectionString_ffi $ toJSStr newTopClass
                   showVisibleColumn sourceColumnIndex' (cg !! sourceColumnIndex')
                   setCallbacks game $ Just newTopClass
               else
                   return ()

onDragEnd :: Game -> Maybe String -> JSString -> JSString -> Int -> Int -> IO ()
onDragEnd game@(Game fg cg _ _) topClass jsCardId jsClassName x y = 
    let draggedToColumn = (y >= yColumnPlacement && x >= xColumnPlacement)
    in if draggedToColumn then 
           let sourceColumnIndex = columnIndexFromJSCardId jsCardId game
               destColumnIndex  = min 6 $ (x - xColumnPlacement) `div` xSep
               isValidMove = 
                   case sourceColumnIndex of 
                      Nothing -> False
                      Just sourceColumnIndex' -> (last.visible $ cg !! sourceColumnIndex') `goesOnColumn` (cg !! destColumnIndex)
               validSourceColumnIndex = fromMaybe 0 sourceColumnIndex 

           in if isValidMove then do
                   let newGame@(Game _ ncg _ _) = fromColumnToColumn game validSourceColumnIndex destColumnIndex
                       newTopClass = (".visibleColumn" ++ show validSourceColumnIndex)
                   alignGame newGame
                   deleteColumn validSourceColumnIndex
                   showColumn validSourceColumnIndex $ ncg !! validSourceColumnIndex
                   setCallbacks newGame $ Just newTopClass
              else -- not a valid move for some reason
                   alignGame game
       else -- drag destination was not on a column.  Was it on a foundation?
           let draggedToFoundation = y < yColumnPlacement && x >= xFoundationPlacement
           in if draggedToFoundation then 
                  let sourceColumnIndex = columnIndexFromJSCardId jsCardId game
                      destFoundationIndex  = min 3 $ (x - xFoundationPlacement) `div` xSep
                      isValidMove = 
                          case sourceColumnIndex of 
                             Nothing -> False
                             Just sourceColumnIndex' -> (last.visible $ cg !! sourceColumnIndex') `goesOnFoundation` (fg !! destFoundationIndex)
                      validSourceColumnIndex = fromMaybe 0 sourceColumnIndex 

                  in if isValidMove then do
                          let newGame@(Game _ ncg _ _) = fromColumnToFoundation game validSourceColumnIndex destFoundationIndex
                          alignGame newGame
                          deleteColumn validSourceColumnIndex
                          showColumn validSourceColumnIndex $ ncg !! validSourceColumnIndex
                          setCallbacks newGame $ topClass -- nothing new on table - no need to change topClass 
                     else -- not a valid move for some reason
                          alignGame game
              else -- drag destination was not on a column or foundation.  
                  alignGame game


loadCallback = do
    shuffledDeck <- shuffle [ Card r s | r<-[Ace .. King], s<-[Hearts .. Clubs]] 
    let
        foundations = replicate 4 []
        columns     = replicate 7 $ Column [] []
        deck        = []
        game        = Game foundations columns deck shuffledDeck
        gameInPlay  = start game
    showGame $ gameInPlay
    setCallbacks gameInPlay Nothing

main = do 
          loadCards_ffi(toPtr loadCallback)
