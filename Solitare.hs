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
foreign import ccall showAlert_ffi :: JSString -> IO ()
foreign import ccall consoleLog_ffi :: JSString -> IO ()

foreign import ccall loadCards_ffi :: Ptr (IO ()) -> IO ()
foreign import ccall placeCard_ffi :: JSString -> JSString -> JSString -> Int -> Int -> IO ()
foreign import ccall deleteByClass_ffi :: JSString -> IO ()
foreign import ccall setMouseoverCallback_ffi :: Ptr (JSString -> JSString -> Int -> Int -> IO ()) -> IO ()
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
    deleteByClass_ffi $ toJSStr ("hiddenColumn"++show hindex)

deleteVisibleColumn :: Int -> IO ()
deleteVisibleColumn hindex = 
    deleteByClass_ffi $ toJSStr ("visibleColumn"++show hindex)

deleteDeck :: IO()
deleteDeck = do
    deleteByClass_ffi $ toJSStr "emptyDeck"
    deleteByClass_ffi $ toJSStr "solitareDeck"

deleteReserves :: IO()
deleteReserves = do
    deleteByClass_ffi $ toJSStr "emptyReserves"
    deleteByClass_ffi $ toJSStr "hiddenReserves"

deleteColumn :: Int -> IO ()
deleteColumn hindex = do
    deleteHiddenColumn hindex 
    deleteVisibleColumn hindex 


-- place card with id, css class, column, depth in column
placeTableCard :: String -> String -> String -> Int -> Int -> IO ()
placeTableCard id name cssClass columnIndex positionInColumn =
        placeCard_ffi (toJSStr id) 
                  (toJSStr name)
                  (toJSStr cssClass)
                  (xColumnPlacement+ xSep*columnIndex) 
                  (yColumnPlacement+ ySep*positionInColumn)

-- place card with id, column on foundation
placeFoundationCard :: String -> String -> String -> Int -> IO ()
placeFoundationCard id name cssClass hindex =
        let vindex = 0
        in placeCard_ffi (toJSStr id) 
                  (toJSStr name)
                  (toJSStr cssClass)
                  (xFoundationPlacement+ xSep*hindex) 
                  (yFoundationPlacement+ ySep*vindex)

-- place card with id, class, on deck
placeDeckCard :: String -> String -> String -> IO ()
placeDeckCard id name cssClass =
    placeCard_ffi (toJSStr id) (toJSStr name) (toJSStr cssClass) xDeckPlacement yDeckPlacement

-- place card with id, class, on reserves
placeReservesCard :: String -> String -> String -> IO ()
placeReservesCard id name cssClass =
    placeCard_ffi (toJSStr id) (toJSStr name) (toJSStr cssClass) xReservesPlacement yReservesPlacement

-- display blanks to indicate where cards go if column is empty.
showEmptyColumn :: Int -> IO ()
showEmptyColumn hindex = 
    placeTableCard "base_only" ("emptyColumn"++show hindex) "emptyColumn" hindex 0

-- assign vindex indicating depth in column to each hidden card in column
-- display back of card in column position specified by (hindex,vindex)
showHiddenColumn :: Int -> Column -> IO ()
showHiddenColumn hindex (Column hidden _) = 
    zipWithM_ ph [0..] hidden
            where ph vindex card = placeTableCard "back" (svgString card) ("hiddenColumn"++show hindex) hindex vindex

-- assign vindex indicating depth in column to each visible card in column
-- display card in column position specified by (hindex,vindex)
showVisibleColumn :: Int -> Column -> IO ()
showVisibleColumn hindex (Column hidden visible) = 
    zipWithM_ pc [length hidden..] (reverse visible)
            where pc vindex card = placeTableCard (svgString card) (svgString card) ("visibleColumn"++show hindex) hindex vindex

-- assign vindex indicating depth in column to each card in column
-- display card in column position specified by (hindex,vindex)
showColumn :: Int -> Column -> IO ()
showColumn hindex column = do
    showEmptyColumn hindex 
    showHiddenColumn hindex column
    showVisibleColumn hindex column

showFoundation :: Int -> [Card] -> IO ()
showFoundation hindex foundation = do
    placeFoundationCard "base_only" ("emptyFoundation"++show hindex) ("emptyFoundation"++show hindex) hindex 
    mapM_ pc $ reverse foundation
        where pc card = placeFoundationCard (svgString card) (svgString card) ("foundation"++show hindex) hindex 

showDeck :: [Card] -> IO ()
showDeck deck = do
    placeDeckCard "base_only" "base_only_deck" "emptyDeck" 
    mapM_ (\card -> placeDeckCard (svgString card) (svgString card) "solitareDeck") $ reverse deck

showReserves :: [Card] -> IO ()
showReserves deck = do
    placeReservesCard "base_only" "base_only_reserves" "emptyReserves" 
    mapM_ (\card -> placeReservesCard "back" (svgString card) "hiddenReserves") deck

showGame :: Game -> IO ()
showGame game@(Game foundations columns deck reserves)  =  do
    mapM_ (uncurry showColumn) (zip [0..] columns)
    mapM_ (uncurry showFoundation) (zip [0..] foundations)
    showDeck deck
    showReserves reserves
         
setCallbacks :: Game -> Maybe String -> IO ()
setCallbacks game topClass = do
        setDragEndCallback_ffi $ toPtr (onDragEnd game topClass)
        setMouseoverCallback_ffi $ toPtr (onMouseover game topClass)

onMouseover :: Game -> Maybe String -> JSString -> JSString -> Int -> Int -> IO ()
onMouseover game@(Game _ cg dg rg) topClass jsCardId jsClass x y = 
    when (differentTopClass && (isVisCol || isRes || isDeck )) $ 
    do
        deleteByClass_ffi $ toJSStr newTopClass
        setCallbacks game $ Just newTopClass
        if isVisCol then
            let sourceColumnIndex = read (fromJust (stripPrefix "visibleColumn" cls)) :: Int
            in showVisibleColumn sourceColumnIndex (cg !! sourceColumnIndex)
        else if isRes then
            showReserves rg
        else if isDeck then
            showDeck dg
        else
            let errorMsg = "In onMouseover - Unhandled id/class: " ++ fromJSStr jsCardId ++ "/" ++ fromJSStr jsClass 
            in consoleLog_ffi $ toJSStr errorMsg
    where
        cls = fromJSStr jsClass
        isVisCol = "visibleColumn" `isPrefixOf` cls 
        isRes = "hiddenReserves" == cls 
        isDeck = "solitareDeck" == cls 
        newTopClass = fromJSStr jsClass
        differentTopClass = Just newTopClass /= topClass

moveFromColumnToColumn :: Game -> Maybe String -> String -> String -> Int -> Int -> Int -> IO ()
moveFromColumnToColumn game@(Game _ cg _ _) topClass cardId cls x y sourceColumnIndex =
   let destColumnIndex  = min 6 $ (x - xColumnPlacement) `div` xSep
       isValidMove = (last.visible $ cg !! sourceColumnIndex) `goesOnColumn` (cg !! destColumnIndex)

   in if isValidMove then do
           let newGame@(Game _ ncg _ _) = fromColumnToColumn game sourceColumnIndex destColumnIndex
               newTopClass = ".visibleColumn" ++ show sourceColumnIndex
           showColumn destColumnIndex $ ncg !! destColumnIndex
           deleteColumn sourceColumnIndex
           showColumn sourceColumnIndex $ ncg !! sourceColumnIndex
           setCallbacks newGame $ Just newTopClass
      else -- not a valid move for some reason
           showColumn sourceColumnIndex $ cg !! sourceColumnIndex

moveFromColumnToFoundation :: Game -> Maybe String -> String -> String -> Int -> Int -> Int -> IO ()
moveFromColumnToFoundation game@(Game fg cg _ _)  topClass cardId cls x y sourceColumnIndex =
    let destFoundationIndex = min 3 $ (x - xFoundationPlacement) `div` xSep
        isValidMove = (head.visible) (cg !! sourceColumnIndex) `goesOnFoundation` (fg !! destFoundationIndex)
    in if isValidMove then do
            let newGame@(Game nfg ncg _ _) = fromColumnToFoundation game sourceColumnIndex destFoundationIndex
            showFoundation destFoundationIndex $ nfg !! destFoundationIndex
            deleteColumn sourceColumnIndex
            showColumn sourceColumnIndex $ ncg !! sourceColumnIndex
            setCallbacks newGame topClass -- nothing new on table - no need to change topClass 
       else -- not a valid move for some reason
            showColumn sourceColumnIndex $ cg !! sourceColumnIndex

moveFromColumn :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromColumn game@(Game _ cg _ _) topClass cardId cls x y 
    | draggedToColumn = moveFromColumnToColumn game topClass cardId cls x y sourceColumnIndex
    | draggedToFoundation  = moveFromColumnToFoundation game topClass cardId cls x y sourceColumnIndex
    | otherwise = showColumn sourceColumnIndex $ cg !! sourceColumnIndex
    where
        sourceColumnIndex = read (fromJust (stripPrefix "visibleColumn" cls)) :: Int
        draggedToColumn = y >= yColumnPlacement && x >= xColumnPlacement
        draggedToFoundation = y < yColumnPlacement && x >= xFoundationPlacement

moveFromReservesToDeck :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromReservesToDeck game topClass cardId cls x y = do
    let newGame@(Game _ _ ndg nrg) = fromReservesToDeck game 
    deleteReserves -- this is to get rid of the face down card that was just dragged.
    showReserves nrg
    deleteDeck
    showDeck ndg
    setCallbacks newGame $ Just ".solitareDeck"

moveFromReserves :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromReserves game@(Game _ cg _ rg) topClass cardId cls x y 
    | draggedToDeck = moveFromReservesToDeck game topClass cardId cls x y 
    | otherwise = showReserves rg
    where
        draggedToDeck = y < yColumnPlacement && x >= xDeckPlacement && x < (xDeckPlacement + xSep)

moveFromDeckToReserves :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromDeckToReserves game topClass cardId cls x y = do
    let newGame@(Game _ _ ndg nrg) = fromDeckToReserves game 
    deleteDeck
    -- showDeck ndg -- no need to show deck since there is none
    deleteReserves -- this is to get rid of the face up card that was just dragged.
    showReserves nrg
    setCallbacks newGame $ Just ".hiddenReserves"

moveFromDeckToColumn :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromDeckToColumn game@(Game _ cg dg _) topClass cardId cls x y = do
    let destColumnIndex  = min 6 $ (x - xColumnPlacement) `div` xSep
        isValidMove = head dg `goesOnColumn` (cg !! destColumnIndex)
        newGame@(Game _ ncg _ _) = fromDeckToColumn game destColumnIndex
    if isValidMove then do
        showColumn destColumnIndex (ncg !! destColumnIndex)
        setCallbacks newGame topClass  -- top class does not change.
    else
        showDeck dg

moveFromDeckToFoundation :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromDeckToFoundation game@(Game fg _ dg _) topClass cardId cls x y = do
    let destFoundationIndex = min 3 $ (x - xFoundationPlacement) `div` xSep
        isValidMove = head dg `goesOnFoundation` (fg !! destFoundationIndex)
        newGame@(Game nfg _ _ _) = fromDeckToFoundation game destFoundationIndex
    if isValidMove then do
        showFoundation destFoundationIndex $ nfg !! destFoundationIndex
        setCallbacks newGame topClass  -- top class does not change.
    else
        showDeck dg

moveFromDeck :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromDeck game@(Game _ cg dg _) topClass cardId cls x y 
    | draggedToReserves = moveFromDeckToReserves game topClass cardId cls x y 
    | draggedToColumn = moveFromDeckToColumn game topClass cardId cls x y 
    | draggedToFoundation = moveFromDeckToFoundation game topClass cardId cls x y 
    | otherwise = showDeck dg
    where
        draggedToReserves = y < yColumnPlacement && x >= xReservesPlacement && x < (xReservesPlacement + xSep)
        draggedToColumn = y >= yColumnPlacement && x >= xColumnPlacement
        draggedToFoundation = y < yColumnPlacement && x >= xFoundationPlacement

onDragEnd :: Game -> Maybe String -> JSString -> JSString -> Int -> Int -> IO ()
onDragEnd game topClass jsCardId jsClass x y 
    | "visibleColumn" `isPrefixOf` cls = moveFromColumn game topClass cardId cls x y 
    | "hiddenReserves" == cls = moveFromReserves game topClass cardId cls x y 
    | "solitareDeck" == cls = moveFromDeck game topClass cardId cls x y 
    | otherwise = do 
          let errorMsg = "In onDragEnd - Unhandled id/class: " ++ fromJSStr jsCardId ++ "/" ++ fromJSStr jsClass 
          consoleLog_ffi $ toJSStr errorMsg
    where cls = fromJSStr jsClass
          cardId  = fromJSStr jsCardId

main = loadCards_ffi(toPtr loadCallback) where
    loadCallback = do
        shuffledDeck <- shuffle [ Card r s | r<-[Ace .. King], 
                                             s<-[Hearts .. Clubs]] 
        let
            foundations = replicate 4 []
            columns     = replicate 7 $ Column [] []
            deck        = []
            game        = Game foundations columns deck shuffledDeck
            gameInPlay  = start game
        showGame gameInPlay
        setCallbacks gameInPlay Nothing

