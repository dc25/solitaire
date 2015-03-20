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
foreign import ccall alignCard_ffi :: JSString -> JSString -> Int -> Int -> IO ()
foreign import ccall deleteBySelectionString_ffi :: JSString -> IO ()
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
    deleteBySelectionString_ffi $ toJSStr (".hiddenColumn"++show hindex)

deleteVisibleColumn :: Int -> IO ()
deleteVisibleColumn hindex = 
    deleteBySelectionString_ffi $ toJSStr (".visibleColumn"++show hindex)

deleteDeck :: IO()
deleteDeck =
    deleteBySelectionString_ffi $ toJSStr (".solitareDeck")

deleteReserves :: IO()
deleteReserves =
    deleteBySelectionString_ffi $ toJSStr (".hiddenReserves")

deleteColumn :: Int -> IO ()
deleteColumn hindex = do
    deleteHiddenColumn hindex 
    deleteVisibleColumn hindex 


-- place card with id, css class, column, depth in column
placeTableCard :: String -> String -> String -> Int -> Int -> IO ()
placeTableCard id name cssClass columnIndex positionInColumn =
        placeCard_ffi (toJSStr id) 
                  (toJSStr id)
                  (toJSStr cssClass)
                  (xColumnPlacement+ xSep*columnIndex) 
                  (yColumnPlacement+ ySep*positionInColumn)

-- place card with id, column on foundation
placeFoundationCard :: String -> String -> Int -> IO ()
placeFoundationCard id cssClass hindex =
        let vindex = 0
        in placeCard_ffi (toJSStr id) 
                  (toJSStr id)
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
    sequence_ (map ph $ zip [0..] hidden)
            where ph (vindex,card) = placeTableCard "back" (svgString card) ("hiddenColumn"++show hindex) hindex vindex

-- assign vindex indicating depth in column to each visible card in column
-- display card in column position specified by (hindex,vindex)
showVisibleColumn :: Int -> Column -> IO ()
showVisibleColumn hindex (Column hidden visible) = 
    sequence_ (map pc $ zip [length hidden..] (reverse visible))
            where pc (vindex,card) = placeTableCard (svgString card) (svgString card) ("visibleColumn"++show hindex) hindex vindex

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
    placeDeckCard "base_only" "base_only" "emptyDeck" 
    sequence_ $ map (\card -> placeDeckCard (svgString card) (svgString card) "solitareDeck") $ reverse deck

showReserves :: [Card] -> IO ()
showReserves deck = do
    placeReservesCard "base_only" "base_only" "emptyReserves" 
    sequence_ $ map (\card -> placeReservesCard "back" (svgString card) "hiddenReserves") deck

showGame :: Game -> IO ()
showGame game@(Game foundations columns deck reserves)  =  do
    sequence_ $ map (uncurry showColumn) (zip [0..] columns)
    sequence_ $ map (uncurry showFoundation) (zip [0..] foundations)
    showDeck deck
    showReserves reserves
         
-- align card with id, css class, column, depth in column
alignTableCard :: String -> String -> Int -> Int -> IO ()
alignTableCard name cssClass columnIndex positionInColumn =
        alignCard_ffi (toJSStr name) 
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
alignFoundationCard name cssClass foundationIndex =
    let positionInFoundation = 0 
    in alignCard_ffi (toJSStr name) 
              (toJSStr cssClass)
              (xFoundationPlacement+ xSep*foundationIndex) 
              (yFoundationPlacement+ ySep*positionInFoundation)

-- display card in foundation position specified by hindex
alignFoundation:: Int -> [Card] -> IO ()
alignFoundation hindex foundation = 
    sequence_ (map pc $ (reverse foundation))
            where pc card = alignFoundationCard (svgString card) ("foundation"++show hindex) hindex 

-- align card in deck with id, css 
alignDeckCard :: String -> String -> IO ()
alignDeckCard name cssClass =
    alignCard_ffi (toJSStr name) (toJSStr cssClass) xReservesPlacement yReservesPlacement

alignDeck :: [Card] -> IO ()
alignDeck deck = do
    sequence_ $ map (\card -> alignDeckCard (svgString card) "hiddenDeck") deck

-- align card in reserves with name, css 
alignReservesCard :: String -> String -> IO ()
alignReservesCard name cssClass =
    alignCard_ffi (toJSStr name) (toJSStr cssClass) xReservesPlacement yReservesPlacement

alignReserves :: [Card] -> IO ()
alignReserves reserves = do
    sequence_ $ map (\card -> alignReservesCard (svgString card) "hiddenReserves") reserves

setCallbacks :: Game -> Maybe String -> IO ()
setCallbacks game topClass = do
        setDragEndCallback_ffi $ (toPtr $ onDragEnd game topClass)
        setMouseoverCallback_ffi $ (toPtr $ onMouseover game topClass)

columnIndexFromJSCardId :: JSString -> Game -> Maybe Int 
columnIndexFromJSCardId jsStr game = do
    cardId <- fromJSString jsStr -- unwrapping a Maybe
    card <- fromSvgString cardId -- unwrapping a Maybe
    columnIndex card game -- wrapping a maybe

onMouseover :: Game -> Maybe String -> JSString -> JSString -> Int -> Int -> IO ()
onMouseover game@(Game _ cg _ _) topClass jsCardId jsClass x y =
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

moveFromColumnToColumn :: Game -> Maybe String -> String -> String -> Int -> Int -> Int -> IO ()
moveFromColumnToColumn game@(Game _ cg _ _) topClass cardId cls x y sourceColumnIndex =
   let destColumnIndex  = min 6 $ (x - xColumnPlacement) `div` xSep
       isValidMove = (last.visible $ cg !! sourceColumnIndex) `goesOnColumn` (cg !! destColumnIndex)

   in if isValidMove then do
           let newGame@(Game _ ncg _ _) = fromColumnToColumn game sourceColumnIndex destColumnIndex
               newTopClass = (".visibleColumn" ++ show sourceColumnIndex)
           alignColumn destColumnIndex $ ncg !! destColumnIndex
           deleteColumn sourceColumnIndex
           showColumn sourceColumnIndex $ ncg !! sourceColumnIndex
           setCallbacks newGame $ Just newTopClass
      else -- not a valid move for some reason
           alignColumn sourceColumnIndex $ cg !! sourceColumnIndex

moveFromColumnToFoundation :: Game -> Maybe String -> String -> String -> Int -> Int -> Int -> IO ()
moveFromColumnToFoundation game@(Game fg cg _ _)  topClass cardId cls x y sourceColumnIndex =
    let destFoundationIndex = min 3 $ (x - xFoundationPlacement) `div` xSep
        isValidMove = (last.visible $ cg !! sourceColumnIndex) `goesOnFoundation` (fg !! destFoundationIndex)
    in if isValidMove then do
            let newGame@(Game nfg ncg _ _) = fromColumnToFoundation game sourceColumnIndex destFoundationIndex
            alignFoundation destFoundationIndex $ nfg !! destFoundationIndex
            deleteColumn sourceColumnIndex
            showColumn sourceColumnIndex $ ncg !! sourceColumnIndex
            setCallbacks newGame $ topClass -- nothing new on table - no need to change topClass 
       else -- not a valid move for some reason
            alignColumn sourceColumnIndex $ cg !! sourceColumnIndex

moveFromColumn :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromColumn game@(Game _ cg _ _) topClass cardId cls x y 
    | draggedToColumn = moveFromColumnToColumn game topClass cardId cls x y sourceColumnIndex
    | draggedToFoundation  = moveFromColumnToFoundation game topClass cardId cls x y sourceColumnIndex
    | otherwise = alignColumn sourceColumnIndex $ cg !! sourceColumnIndex
    where
        sourceColumnIndex = read (fromJust (stripPrefix "visibleColumn" cls)) :: Int
        draggedToColumn = (y >= yColumnPlacement && x >= xColumnPlacement)
        draggedToFoundation = y < yColumnPlacement && x >= xFoundationPlacement

moveFromReservesToDeck :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromReservesToDeck game@(Game _ _ _ _) topClass cardId cls x y = do
    let newGame@(Game _ _ ndg nrg) = fromReservesToDeck game 
    deleteReserves -- this is to get rid of the face down card that was just dragged.
    showReserves nrg
    deleteDeck
    showDeck ndg
    setCallbacks newGame $ Just (".solitareDeck")

moveFromReserves :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromReserves game@(Game _ cg _ rg) topClass cardId cls x y 
    | draggedToDeck = moveFromReservesToDeck game topClass cardId cls x y 
    | otherwise = alignReserves rg
    where
        draggedToDeck = y < yColumnPlacement && x >= xDeckPlacement && x < (xDeckPlacement + xSep)

moveFromDeckToReserves :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromDeckToReserves game@(Game _ _ _ _) topClass cardId cls x y = do
    let newGame@(Game _ _ ndg nrg) = fromDeckToReserves game 
    deleteDeck
    -- showDeck ndg -- no need to show deck since there is none
    deleteReserves -- this is to get rid of the face up card that was just dragged.
    showReserves nrg
    setCallbacks newGame $ Just (".hiddenReserves")

moveFromDeck :: Game -> Maybe String -> String -> String -> Int -> Int -> IO ()
moveFromDeck game@(Game _ cg _ rg) topClass cardId cls x y 
    | draggedToReserves = moveFromDeckToReserves game topClass cardId cls x y 
    | otherwise = alignReserves rg
    where
        draggedToReserves = y < yColumnPlacement && x >= xReservesPlacement && x < (xReservesPlacement + xSep)
        draggedToColumn = (y >= yColumnPlacement && x >= xColumnPlacement)
        draggedToFoundation = y < yColumnPlacement && x >= xFoundationPlacement

onDragEnd :: Game -> Maybe String -> JSString -> JSString -> Int -> Int -> IO ()
onDragEnd game topClass jsCardId jsClass x y 
    | "visibleColumn" `isPrefixOf` cls = moveFromColumn game topClass cardId cls x y 
    | "hiddenReserves" == cls = moveFromReserves game topClass cardId cls x y 
    | "solitareDeck" == cls = moveFromDeck game topClass cardId cls x y 
    | otherwise = do 
          let errorMsg = "In onDragEnd - Unhandled id/class: " ++ (fromJSStr jsCardId) ++ "/" ++ (fromJSStr jsClass) 
          consoleLog_ffi $ toJSStr errorMsg
    where cls = fromJSStr jsClass
          cardId  = fromJSStr jsCardId

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
