module Game (
    Game(..),
    Column(..),
    start,
    goesOnFoundation,
    goesOnColumn,
    columnIndex,
    fromColumnToFoundation,
    fromColumnToColumn,
    fromDeckToColumn,
    fromDeckToFoundation,
    fromReservesToDeck,
    fromDeckToReserves,
    gameOver
) where

import Data.List
import Card

data Column = Column {
              concealed :: [Card],
              visible ::   [Card]
              }

data Game = Game 
         [[Card]]  -- foundations
         [Column]  -- columns
         [Card]    -- deck
         [Card]    -- reserves

instance Show Game where
    show (Game fg cg dg rg) = 
          let emptySpace = "__"
              hiddenCard = "??"
              noCard =     "  "

              flines = unwords $ map (\f -> if null f then emptySpace else show $ head f) fg
              clines = unwords $ map (\f -> if null $ concealed f then emptySpace else hiddenCard) cg

              vlines = toVisibleLines $ map (reverse.visible) cg where
                   toVisibleLines [[],[],[],[],[],[],[]] = []
                   toVisibleLines vg =
                         unwords (map (\f -> if null f then noCard else show $ head f) vg) 
                       : toVisibleLines (map (\f -> if null f then [] else tail f) vg)

              dlines = unwords [ if null rg then emptySpace else hiddenCard, 
                               if null dg then emptySpace else show $ head dg ]

              lines' = flines:clines:vlines ++ [dlines]
          in unlines lines'

gameOver :: Game -> Bool
gameOver (Game _ cg dg rg) =    
                   all null (map visible cg)
                && all null (map concealed cg)
                && null dg
                && null rg


---------------------------------------------------------
-- The following routines operate on one part 
-- (columns or foundations or deck or reserves ) 
-- of a game.
---------------------------------------------------------

-- Utility functions
goesOnColumn :: Card -> Column -> Bool
goesOnColumn card (Column [] []) = rank card == King
goesOnColumn card (Column _ (vh:_)) = 
    (cardColor card /= cardColor vh) && fromEnum (rank card) + 1 == fromEnum (rank vh)

goesOnFoundation :: Card -> [Card] -> Bool
goesOnFoundation card [] = rank card == Ace 
goesOnFoundation card (fh:_) =
    (suit card == suit fh) && fromEnum (rank card) == fromEnum (rank fh) + 1

-- find the index of the column containing the Card in its visible section
columnIndex :: Card -> [Column] -> Maybe Int
columnIndex card cg = findIndex (elem card) $ map visible cg

-- if the visible portion of a column is empty
-- then "replenish" it with one card from the concealed
-- portion of the column
replenishColumn :: Column -> Column
replenishColumn (Column (ch:ct)  []) = Column ct [ch]
replenishColumn column = column

-- check for empty column has already been done; assert ??
removeOneFromColumn :: [Column] -> Int -> ([Column], Card)
removeOneFromColumn cg index0 =
    let (Column concealed0 visible0) = cg !! index0
        newColumn0 = Column concealed0 (tail visible0)
        newColumn1 = replenishColumn newColumn0
        newColumns0 = take index0 cg ++ newColumn1 : drop (index0+1) cg
    in (newColumns0, head visible0)

-- check for empty column has already been done; assert ??
removeAllFromColumn :: [Column] -> Int -> ([Column], [Card])
removeAllFromColumn cg index0 =
    let (Column concealed0 visible0) = cg !! index0
        newColumn0 = Column concealed0 []
        newColumn1 = replenishColumn newColumn0
        newColumns0 = take index0 cg ++ newColumn1 : drop (index0+1) cg
    in (newColumns0, visible0)

addToFoundations :: [[Card]] -> Int -> Card -> [[Card]]
addToFoundations fg index1 card =
    let foundation1 = fg !! index1
        newFoundation = card : foundation1
    in take index1 fg ++ newFoundation : drop (index1+1) fg

addToColumns :: [Column] -> Int -> [Card] -> [Column]
addToColumns cg index1 cards =
    let (Column concealed1 visible1) = cg !! index1
        newColumn = Column concealed1 (cards ++ visible1)
    in take index1 cg ++ newColumn : drop (index1+1) cg

addOneToColumns :: [Column] -> Int -> Card -> [Column]
addOneToColumns cg index1 card =
    let (Column concealed1 visible1) = cg !! index1
        newColumn = Column concealed1 (card : visible1)
    in take index1 cg ++ newColumn : drop (index1+1) cg

---------------------------------------------------------
-- The following routines operate on an entire game.
---------------------------------------------------------

-- deal a deck of cards out to the klondike layout
start :: Game -> Game
start (Game fg cg dg rg) = 
            let (columns', reserves') = deal cg rg
            in Game fg columns' dg reserves'
            where 
                -- stop dealing out cards when all stacks full
                deal columns'@[] reserves' = (columns', reserves')

                deal columns' reserves' = 
                    let 
                        -- one card up on first stack
                        visibleHead = head reserves' : visible (head columns')

                        -- no concealed cards on first stack (unchanged)
                        concealedHead = concealed $ head columns'

                        -- no cards up after first stack (unchanged)
                        visibleTail = map visible $ tail columns'

                        -- add concealed card to every stack past first
                        concealedTail = zipWith (:) (tail reserves') $ map concealed (tail columns') 

                        -- combine concealed and visible to get column
                        columnsHead = Column concealedHead visibleHead

                        -- zip concealed & visible to get columns'
                        columnsTail = zipWith Column concealedTail visibleTail

                        -- remove used cards from reserves'
                        remainingDeck = drop (length columns') reserves'

                        -- recurse to deal onto remaining columns'
                        (columns'', reserves'') = deal columnsTail remainingDeck

                    in (columnsHead : columns'', reserves'')

fromColumnToFoundation :: Game -> Int -> Int -> Game
fromColumnToFoundation (Game fg cg dg rg) index0 index1 =
    let (newColumns0,removedCard) = removeOneFromColumn cg index0
        newFoundations = addToFoundations fg index1 removedCard
    in Game newFoundations  newColumns0 dg rg 

fromColumnToColumn :: Game -> Int -> Int -> Game
fromColumnToColumn (Game fg cg dg rg) index0 index1 =
    let (newColumns0,removedCards) = removeAllFromColumn cg index0
        newColumns1 = addToColumns newColumns0 index1 removedCards
    in Game fg  newColumns1 dg rg 

fromDeckToColumn :: Game -> Int -> Game
fromDeckToColumn (Game fg cg dg rg) index1 =
    let newDeck = tail dg
        removedCard = head dg
        newColumns = addOneToColumns cg index1 removedCard
    in  Game fg newColumns newDeck rg

fromDeckToFoundation :: Game -> Int -> Game
fromDeckToFoundation (Game fg cg dg rg) index1 =
    let newDeck = tail dg
        removedCard = head dg
        newFoundations = addToFoundations fg index1 removedCard
    in Game newFoundations  cg newDeck rg

fromReservesToDeck:: Game -> Game
fromReservesToDeck (Game fg cg dg rg) = 
    Game fg cg (reverse (take 3 rg) ++ dg) (drop 3 rg)

fromDeckToReserves :: Game -> Game
fromDeckToReserves (Game fg cg dg _) = 
    Game fg cg [] (reverse dg)

