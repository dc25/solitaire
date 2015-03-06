import System.Random
import Data.Char
import Control.Monad

shuffle :: [a] -> IO [a]
shuffle ys = do
    gen <- newStdGen
    return $ shuffle' gen ys where
        shuffle' _ [] = []
        shuffle' gen ys = 
            let (r, newGen) = randomR (0, length ys - 1) gen
                (a,b) = splitAt r ys
            in head b : shuffle' newGen (a ++ tail b)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving  (Eq, Ord, Enum, Bounded, Show, Read)

rankLetter :: Rank -> Char
rankLetter Ace =   'A'
rankLetter Ten =   'T'
rankLetter Jack =  'J'
rankLetter Queen = 'Q'
rankLetter King =  'K'
rankLetter r = chr ((fromEnum r - fromEnum Two) + ord '2')

data Suit = Hearts | Diamonds | Spades | Clubs deriving  (Eq, Ord, Enum, Bounded, Show, Read)

suitLetter :: Suit -> Char
suitLetter Hearts =   'H'
suitLetter Diamonds = 'D'
suitLetter Spades =   'S'
suitLetter Clubs =    'C'

data Color = Red | Black deriving  (Eq, Ord, Enum, Bounded, Show, Read)

suitColor Hearts =   Red
suitColor Diamonds = Red
suitColor Spades =   Black
suitColor Clubs =    Black


data Card = Card {
         rank :: Rank,
         suit :: Suit 
         } 

cardColor (Card _ suit) = suitColor suit


instance Show Card where
    show (Card rank suit) = [rankLetter rank, suitLetter suit]

data Column = Column {
              concealed :: [Card],
              visible ::   [Card]
              }

data Game = Game {
         foundations :: [[Card]],
         columns ::     [Column],
         waste ::       [Card],
         deck ::        [Card]
         } 

instance Show Game where
    show game@(Game fg cg wg dg) = 

          let emptySpace = "__"
              hiddenCard = "??"
              noCard =     "  "

              flines = unwords $ map (\f -> if null f then emptySpace else show $ head f) fg
              clines = unwords $ map (\f -> if null $ concealed f then emptySpace else hiddenCard) cg

              vlines = toVisibleLines $ map (reverse.visible) cg where
                   toVisibleLines vg 
                        = if all null vg then [] 
                          else   unwords (map (\f -> if null f then noCard else show $ head f) vg) 
                               : toVisibleLines (map (\f -> if null f then [] else tail f) vg)

              dlines = unwords [ if null $ deck game then emptySpace else hiddenCard, 
                               if null $ waste game then emptySpace else show $ head $ waste game ]

              lines = flines:clines:vlines ++ [dlines]
          in unlines lines

gameOver :: Game -> Bool
gameOver game@(Game fg cg wg dg) =    
                   all null (map visible cg)
                && all null (map concealed cg)
                && null wg
                && null dg


-- deal a deck of cards out to the klondike layout
start game@(Game fg cg wg dg) = 
             let (columns', deck') = deal cg dg
             in Game fg columns' wg deck'
             where deal columns deck = 
                       -- stop dealing out cards when all stacks full
                       if null columns then (columns, deck) 

                       -- deal out one more row of cards 
                       -- and recurse for remaining rows
                       else let 
                                -- one card up on first stack
                                visibleHead = head deck : visible (head columns)

                                -- no concealed cards on first stack (unchanged)
                                concealedHead = concealed $ head columns

                                -- no cards up after first stack (unchanged)
                                visibleTail = map visible $ tail columns

                                -- add concealed card to every stack past first
                                concealedTail = zipWith (:) (tail deck) $ map concealed (tail columns) 

                                -- combine concealed and visible to get column
                                columnsHead = Column concealedHead visibleHead

                                -- zip concealed & visible to get columns
                                columnsTail = zipWith Column concealedTail visibleTail

                                -- remove used cards from deck
                                remainingDeck = drop (length columns) deck

                                -- recurse to deal onto remaining columns
                                (columns', deck') = deal columnsTail remainingDeck

                            in (columnsHead : columns', deck')

main = do 
          shuffledDeck <- shuffle [ Card r s | r<-[Ace .. King], s<-[Hearts .. Clubs]] 

          let
              foundations = [[],[],[],[]]
              columns =     [ Column [] [], Column [] [], Column [] [], Column [] [], Column [] [], Column [] [], Column [] [] ]
              waste =       []
              game = Game foundations columns waste shuffledDeck
              gameInPlay = start game

          print gameInPlay
          updateLoop gameInPlay
          putStrLn "Game Over"

goesOnColumn card column@(Column concealed visible) = 
       (null visible  && null concealed && rank card == King ) 
    || (not (null visible) && card `goesOn` head visible) where
            goesOn card0 card1 = 
                   (cardColor card0 /= cardColor card1) 
                && fromEnum (rank card0) + 1 == fromEnum (rank card1)

goesOnFoundation card foundation =
       (null foundation  && rank card == Ace ) 
    || (not (null foundation) && card `goesOn` head foundation) where
            goesOn card0 card1 = 
                   (suit card0 == suit card1) 
                && fromEnum (rank card0) == fromEnum (rank card1) + 1

playColumnToFoundation :: Game -> Int -> Int -> IO Game
playColumnToFoundation game@(Game fg cg wg dg) index0 index1 
                | null.visible $ cg !! index0 = do
                    putStrLn $ "Can not move cards from empty column: " ++ show (index0+1)
                    return game

                | not $ (head.visible $ cg !! index0) `goesOnFoundation` (fg !! index1) = do
                    putStrLn $ "Can not move card from column: " ++ show (index0+1) ++ " to foundation: " ++ [chr (ord 'A' + index1)]
                    return game
                  
                | (null.concealed) (cg !! index0) = do
                    let newColumn0 = Column [] []
                    let newColumns0 = take index0 cg ++ newColumn0 : drop (index0+1) cg

                    let newFoundation = (head.visible) (cg !! index0) : (fg !! index1)
                    let newFoundations = take index1 fg ++ newFoundation : drop (index1+1) fg

                    let newGame = Game newFoundations  newColumns0 wg dg 
                    print newGame 
                    return newGame 

                | otherwise = do
                    let newColumn0 = Column (tail.concealed $ cg !! index0) [head.concealed $ cg !! index0]
                    let newColumns0 = take index0 cg ++ newColumn0 : drop (index0+1) cg

                    let newFoundation = (head.visible) (cg !! index0) : (fg !! index1)
                    let newFoundations = take index1 fg ++ newFoundation : drop (index1+1) fg

                    let newGame = Game newFoundations  newColumns0 wg dg 
                    print newGame 
                    return newGame 

playColumnToColumn :: Game -> Int -> Int -> IO Game
playColumnToColumn game@(Game fg cg wg dg) index0 index1 
                | index0 == index1 = do
                    putStrLn "Can not move cards from a column to itself."
                    return game

                | null.visible $ cg !! index0 = do
                    putStrLn $ "Can not move cards from empty column: " ++ show (index0+1)
                    return game

                | not $ (last.visible $ cg !! index0) `goesOnColumn` (cg !! index1) = do
                    putStrLn $ "Can not move cards from column: " ++ show (index0+1) ++ " to column: " ++ show (index1+1)
                    return game
                  
                | (null.concealed) (cg !! index0) = do
                    let newColumn0 = Column [] []
                    let newColumns0 = take index0 cg ++ newColumn0 : drop (index0+1) cg

                    let newColumn1 = Column (concealed $ cg !! index1) (visible (cg !! index0) ++ visible (cg !! index1))
                    let newColumns1 = take index1 newColumns0 ++ newColumn1 : drop (index1+1) newColumns0

                    let newGame = Game fg  newColumns1 wg dg 
                    print newGame 
                    return newGame 

                | otherwise = do
                    let newColumn0 = Column (tail.concealed $ cg !! index0) [head.concealed $ cg !! index0]
                    let newColumns0 = take index0 cg ++ newColumn0 : drop (index0+1) cg

                    let newColumn1 = Column (concealed $ cg !! index1) (visible (cg !! index0) ++ visible (cg !! index1))
                    let newColumns1 = take index1 newColumns0 ++ newColumn1 : drop (index1+1) newColumns0

                    let newGame = Game fg  newColumns1 wg dg 
                    print newGame 
                    return newGame 

playDeckToColumn :: Game -> Int -> IO Game
playDeckToColumn game@(Game fg cg wg dg) index1 
                | null wg = do
                    putStrLn "Can not play from empty deck." 
                    return game

                | not $ head wg `goesOnColumn` (cg !! index1) = do
                    putStrLn $ "Can not move cards from deck to column: " ++ show (index1+1)
                    return game
                  
                | otherwise = do
                    let newDeck = tail wg

                    let newColumn1 = Column (concealed $ cg !! index1) (head wg : visible (cg !! index1))
                    let newColumns1 = take index1 cg ++ newColumn1 : drop (index1+1) cg

                    let newGame = Game fg  newColumns1 newDeck dg
                    print newGame 
                    return newGame 

playDeckToFoundation :: Game -> Int -> IO Game
playDeckToFoundation game@(Game fg cg wg dg) index1 
                | null wg = do
                    putStrLn "Can not play from empty deck." 
                    return game

                | not $ head wg `goesOnFoundation` (fg !! index1) = do
                    putStrLn $ "Can not move cards from deck to foundation: " ++ [chr (ord 'A' + index1)]
                    return game
                  
                | otherwise = do
                    let newDeck = tail wg

                    let newFoundation = head wg : (fg !! index1)
                    let newFoundations = take index1 fg ++ newFoundation : drop (index1+1) fg

                    let newGame = Game newFoundations cg newDeck dg
                    print newGame 
                    return newGame 


playFromTable :: Game -> Char -> Char -> IO Game
playFromTable game@(Game fg cg wg dg) cmd0 cmd1 
          | cmd1 >= '1' && cmd1 <= '7' = 
              playColumnToColumn game (ord cmd0 - ord '1') (ord cmd1 - ord '1') 
          | cmd1 >= 'A' && cmd1 <= 'D' = 
              playColumnToFoundation game (ord cmd0 - ord '1') (ord cmd1 - ord 'A') 
  
playFromDeck :: Game -> Char -> IO Game
playFromDeck game@(Game fg cg wg dg) cmd1 
          | cmd1 >= '1' && cmd1 <= '7' =
              playDeckToColumn game (ord cmd1 - ord '1') 
          | cmd1 >= 'A' && cmd1 <= 'D' = 
              playDeckToFoundation game (ord cmd1 - ord 'A') 

updateGame :: Game -> String -> IO Game
updateGame game@(Game fg cg wg dg)  command
        | cmd0 == 'D' = if null dg then do 
                            putStrLn "No cards are available to Draw from.  Use 'R' to replenish."
                            return game
                        else do
                            let newGame = Game fg cg (reverse (take 3 dg) ++ wg) (drop 3 dg)
                            print newGame 
                            return newGame

        | cmd0 == 'R' = if not (null dg) then do 
                            putStrLn "Can not Replenish deck while it still contains cards."
                            return game
                        else do
                            let newGame = Game fg cg [] (reverse wg)
                            print newGame 
                            return newGame

        | cmd0 == 'P' = playFromDeck game cmd1

        | cmd0 >= '1' && cmd0 <= '7' = playFromTable game cmd0 cmd1

        | otherwise = do putStrLn $ "Invalid command: " ++ command
                         return $ Game fg cg wg dg

        where cmd0 = head command
              cmd1 = if length command > 1 then head $ tail command else ' '

updateLoop :: Game -> IO ()
updateLoop game = 
    Control.Monad.unless (gameOver game) $ do
        command <- getLine
        updatedGame <- updateGame game command
        updateLoop updatedGame

