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
foreign import ccall placeAlert_ffi :: JSString -> IO ()
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

hiddenColumnPrefix    = "hiddenColumn"
visibleColumnPrefix   = "visibleColumn"
emptyColumnClass      = "emptyColumn"
emptyDeckClass        = "emptyDeck"
deckClass             = "solitareDeck"
emptyReservesClass    = "emptyReserves"
reservesClass         = "hiddenReserves"
emptyFoundationClass  = "emptyFoundationClass"
foundationClassPrefix = "foundation"

deleteHiddenColumn :: Int -> IO ()
deleteHiddenColumn hindex = 
    deleteByClass_ffi $ toJSStr (hiddenColumnPrefix++show hindex)

deleteVisibleColumn :: Int -> IO ()
deleteVisibleColumn hindex = 
    deleteByClass_ffi $ toJSStr (visibleColumnPrefix++show hindex)

deleteDeck :: IO()
deleteDeck = do
    deleteByClass_ffi $ toJSStr emptyDeckClass
    deleteByClass_ffi $ toJSStr deckClass

deleteReserves :: IO()
deleteReserves = do
    deleteByClass_ffi $ toJSStr emptyReservesClass
    deleteByClass_ffi $ toJSStr reservesClass

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
placeEmptyColumn :: Int -> IO ()
placeEmptyColumn hindex = 
    placeTableCard "base_only" (emptyColumnClass++show hindex) emptyColumnClass hindex 0

-- assign vindex indicating depth in column to each hidden card in column
-- display back of card in column position specified by (hindex,vindex)
placeHiddenColumn :: Int -> Column -> IO ()
placeHiddenColumn hindex (Column hidden _) = 
    zipWithM_ ph [0..] hidden
            where ph vindex card = placeTableCard "back" (svgString card) (hiddenColumnPrefix++show hindex) hindex vindex

-- assign vindex indicating depth in column to each visible card in column
-- display card in column position specified by (hindex,vindex)
placeVisibleColumn :: Int -> Column -> IO ()
placeVisibleColumn hindex (Column hidden visible) = 
    zipWithM_ pc [length hidden..] (reverse visible)
            where pc vindex card = placeTableCard (svgString card) (svgString card) (visibleColumnPrefix++show hindex) hindex vindex

-- assign vindex indicating depth in column to each card in column
-- display card in column position specified by (hindex,vindex)
placeColumn :: Int -> Column -> IO ()
placeColumn hindex column = do
    placeEmptyColumn hindex 
    placeHiddenColumn hindex column
    placeVisibleColumn hindex column

placeFoundation :: Int -> [Card] -> IO ()
placeFoundation hindex foundation = do
    placeFoundationCard "base_only" (emptyFoundationClass++show hindex) emptyFoundationClass hindex 
    mapM_ pc $ reverse foundation
        where pc card = placeFoundationCard (svgString card) (svgString card) (foundationClassPrefix++show hindex) hindex 

placeDeck :: [Card] -> IO ()
placeDeck deck = do
    placeDeckCard "base_only" "base_only_deck" emptyDeckClass
    mapM_ (\card -> placeDeckCard (svgString card) (svgString card) deckClass) $ reverse deck

placeReserves :: [Card] -> IO ()
placeReserves deck = do
    placeReservesCard "base_only" "base_only_reserves" emptyReservesClass 
    mapM_ (\card -> placeReservesCard "back" (svgString card) reservesClass) deck

placeGame :: Game -> IO ()
placeGame game@(Game foundations columns deck reserves)  =  do
    mapM_ (uncurry placeColumn) (zip [0..] columns)
    mapM_ (uncurry placeFoundation) (zip [0..] foundations)
    placeDeck deck
    placeReserves reserves
         
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
            let sourceColumnIndex = read (fromJust (stripPrefix visibleColumnPrefix cls)) :: Int
            in placeVisibleColumn sourceColumnIndex (cg !! sourceColumnIndex)
        else if isRes then
            placeReserves rg
        else if isDeck then
            placeDeck dg
        else
            let errorMsg = "In onMouseover - Unhandled id/class: " ++ fromJSStr jsCardId ++ "/" ++ fromJSStr jsClass 
            in consoleLog_ffi $ toJSStr errorMsg
    where
        cls = fromJSStr jsClass
        isVisCol = visibleColumnPrefix `isPrefixOf` cls 
        isRes = reservesClass == cls 
        isDeck = deckClass == cls 
        newTopClass = fromJSStr jsClass
        differentTopClass = Just newTopClass /= topClass

moveFromColumnToColumn :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromColumnToColumn game@(Game _ cg _ _) topClass cardId cls x y =
   let destColumnIndex  = min (length cg - 1) $ (x - xColumnPlacement) `div` xSep
       isValidMove = (last.visible $ cg !! sourceColumnIndex) `goesOnColumn` (cg !! destColumnIndex)
       sourceColumnIndex = read (fromJust (stripPrefix visibleColumnPrefix cls)) :: Int

   in if isValidMove then do
           let newGame@(Game _ ncg _ _) = fromColumnToColumn game sourceColumnIndex destColumnIndex
               newTopClass = visibleColumnPrefix ++ show sourceColumnIndex
           placeColumn destColumnIndex $ ncg !! destColumnIndex
           deleteColumn sourceColumnIndex
           placeColumn sourceColumnIndex $ ncg !! sourceColumnIndex
           setCallbacks newGame $ Just newTopClass
      else -- not a valid move for some reason
           placeColumn sourceColumnIndex $ cg !! sourceColumnIndex

moveFromColumnToFoundation :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromColumnToFoundation game@(Game fg cg _ _)  topClass cardId cls x y =
    let destFoundationIndex = min (length fg - 1) $ (x - xFoundationPlacement) `div` xSep
        isValidMove = (head.visible) (cg !! sourceColumnIndex) `goesOnFoundation` (fg !! destFoundationIndex)
        sourceColumnIndex = read (fromJust (stripPrefix visibleColumnPrefix cls)) :: Int
    in if isValidMove then do
            let newGame@(Game nfg ncg _ _) = fromColumnToFoundation game sourceColumnIndex destFoundationIndex
            placeFoundation destFoundationIndex $ nfg !! destFoundationIndex
            deleteColumn sourceColumnIndex
            placeColumn sourceColumnIndex $ ncg !! sourceColumnIndex
            setCallbacks newGame topClass -- nothing new on table - no need to change topClass 
       else -- not a valid move for some reason
            placeColumn sourceColumnIndex $ cg !! sourceColumnIndex

moveFromReservesToDeck :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromReservesToDeck game topClass cardId cls x y = do
    let newGame@(Game _ _ ndg nrg) = fromReservesToDeck game 
    deleteReserves -- this is to get rid of the face down card that was just dragged.
    placeReserves nrg
    deleteDeck
    placeDeck ndg
    setCallbacks newGame $ Just deckClass

moveFromDeckToReserves :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromDeckToReserves game topClass cardId cls x y = do
    let newGame@(Game _ _ ndg nrg) = fromDeckToReserves game 
    deleteDeck
    -- placeDeck ndg -- no need to place deck since there is none
    deleteReserves -- this is to get rid of the face up card that was just dragged.
    placeReserves nrg
    setCallbacks newGame $ Just reservesClass

moveFromDeckToColumn :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromDeckToColumn game@(Game _ cg dg _) topClass cardId cls x y = do
    let destColumnIndex  = min 6 $ (x - xColumnPlacement) `div` xSep
        isValidMove = head dg `goesOnColumn` (cg !! destColumnIndex)
        newGame@(Game _ ncg _ _) = fromDeckToColumn game destColumnIndex
    if isValidMove then do
        placeColumn destColumnIndex (ncg !! destColumnIndex)
        setCallbacks newGame topClass  -- top class does not change.
    else
        placeDeck dg

moveFromDeckToFoundation :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromDeckToFoundation game@(Game fg _ dg _) topClass cardId cls x y = do
    let destFoundationIndex = min 3 $ (x - xFoundationPlacement) `div` xSep
        isValidMove = head dg `goesOnFoundation` (fg !! destFoundationIndex)
        newGame@(Game nfg _ _ _) = fromDeckToFoundation game destFoundationIndex
    if isValidMove then do
        placeFoundation destFoundationIndex $ nfg !! destFoundationIndex
        setCallbacks newGame topClass  -- top class does not change.
    else
        placeDeck dg

onDragEnd :: Game -> Maybe String -> JSString -> JSString -> Int -> Int -> IO ()
onDragEnd game topClass jsCardId jsClass x y 
    | fromColumn && toColumn     = moveFromColumnToColumn game topClass cardId cls x y 
    | fromColumn && toFoundation = moveFromColumnToFoundation game topClass cardId cls x y 
    | fromReserves && toDeck     = moveFromReservesToDeck game topClass cardId cls x y 
    | fromDeck && toReserves     = moveFromDeckToReserves game topClass cardId cls x y 
    | fromDeck && toColumn       = moveFromDeckToColumn game topClass cardId cls x y 
    | fromDeck && toFoundation   = moveFromDeckToFoundation game topClass cardId cls x y 
    | otherwise                  = placeGame game
    where cls = fromJSStr jsClass
          cardId  = fromJSStr jsCardId

          fromColumn = visibleColumnPrefix `isPrefixOf` cls 
          fromReserves = reservesClass == cls 
          fromDeck = deckClass == cls 

          toReserves = y < yColumnPlacement && x >= xReservesPlacement && x < (xReservesPlacement + xSep)
          toColumn = y >= yColumnPlacement && x >= xColumnPlacement
          toFoundation = y < yColumnPlacement && x >= xFoundationPlacement
          toDeck = y < yColumnPlacement && x >= xDeckPlacement && x < (xDeckPlacement + xSep)


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
        placeGame gameInPlay
        setCallbacks gameInPlay Nothing

