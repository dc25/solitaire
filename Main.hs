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

data Game = Game {
         foundations :: [[Card]],
         concealed ::   [[Card]],
         visible ::     [[Card]],
         waste ::       [Card],
         deck ::        [Card]
         }

display :: Game -> IO ()
display game = do 

      let emptySpace = "__"
      let hiddenCard = "??"
      let noCard =     "  "

      putStrLn $ unwords $ map (\f -> if null f then emptySpace else show $ head f) $ foundations game
      putStrLn $ unwords $ map (\f -> if null f then emptySpace else hiddenCard) $ concealed game

      -- reverse visible stacks of cards to display bottom cards first
      let vl = toVisibleLines $ map reverse $ visible game where
           toVisibleLines vg 
                = if all null vg then [] 
                  else   unwords (map (\f -> if null f then noCard else show $ head f) vg) 
                       : toVisibleLines (map (\f -> if null f then [] else tail f) vg)

      mapM_ putStrLn vl

      putStrLn $ unwords [ if null $ deck game then emptySpace else hiddenCard, 
                           if null $ waste game then emptySpace else show $ head $ waste game ]

gameOver :: Game -> Bool
gameOver game =    all null (visible game) 
                && all null (concealed game) 
                && null (waste game) 
                && null (deck game) 


-- deal a deck of cards out to the klondike layout
start game = let (visible', concealed', deck') = deal (visible game) (concealed game) (deck game)
             in Game (foundations game) concealed' visible' (waste game) deck'
             where deal visible concealed deck = 
                       -- stop dealing out cards when all stacks full
                       if null visible then (visible, concealed, deck) 

                       -- deal out one more row of cards 
                       -- and recurse for remaining rows
                       else let 
                                -- one card up on first stack
                                visibleHead = (head deck : head visible) 

                                -- no concealed cards on first stack (unchanged)
                                concealedHead = head concealed 

                                -- no cards up after first stack (unchanged)
                                visibleTail = tail visible 

                                -- add concealed card to every stack past first
                                concealedTail = zipWith (:) (tail deck) (tail concealed) 

                                -- remove used cards from deck
                                remainingDeck = drop (length visible) deck

                                -- recurse to deal onto visible/concealed tail
                                (visibleTail', concealedTail', deck') 
                                    = deal visibleTail concealedTail remainingDeck

                             in (visibleHead : visibleTail', concealedHead : concealedTail', deck')

main = do 
          shuffledDeck <- shuffle [ Card r s | r<-[Ace .. King], s<-[Hearts .. Clubs]] 

          let
              foundations = [[],[],[],[]]
              concealed =   [[],[],[],[],[],[],[]]
              visible =     [[],[],[],[],[],[],[]]
              waste =       []
              game = Game foundations concealed visible waste shuffledDeck
              gameInPlay = start game

          display gameInPlay
          updateLoop gameInPlay
          putStrLn "Game Over"

goesOn :: Card -> Card -> Bool
goesOn c0 c1 = (cardColor c0 /= cardColor c1) && fromEnum (rank c0) + 1 == fromEnum (rank c1)


goesOnColumn card concealed visible = 
       (null visible  && null concealed && rank card == King ) 
    || (not (null visible) && card `goesOn` head visible) 

playFromDeck :: Game -> Char -> IO Game
playFromDeck game@(Game fg cg vg wg dg) subCommand 
        | subCommand >= '1' && subCommand <= '7'
            = let tableIndex = ord subCommand - ord '1'
                  visibleColumn = vg !! tableIndex
                  concealedColumn = cg !! tableIndex
                  tableCard = head visibleColumn
                  deckCard = head (waste game)
              in if goesOnColumn deckCard concealedColumn visibleColumn then do
                     putStrLn $ "Putting " ++ show deckCard ++ " on column# " ++ show tableIndex
                     let newGame = Game fg cg (take tableIndex vg ++ (deckCard : visibleColumn) : drop (tableIndex+1) vg) (drop 1 wg) dg
                     display newGame
                     return newGame
                 else do
                     putStrLn $ "Can not put " ++ show deckCard ++ " on column# " ++ show tableIndex
                     return game



playFromTable :: Game -> Char -> IO Game
playFromTable game subCommand = return game

updateGame :: Game -> String -> IO Game
updateGame game@(Game fg cg vg wg dg)  command
        | cmd0 == 'D' = if null dg then do 
                            putStrLn "No cards are available to Draw from.  Use 'R' to replenish."
                            return game
                        else do
                            let newGame = Game fg cg vg (reverse (take 3 dg) ++ wg) (drop 3 dg)
                            display newGame 
                            return newGame

        | cmd0 == 'R' = if not (null dg) then do 
                            putStrLn "Can not Replenish deck while it still contains cards."
                            return game
                        else do
                            let newGame = Game fg cg vg [] (reverse wg)
                            display newGame 
                            return newGame

        | cmd0 == 'P' = playFromDeck game cmd1

        | cmd0 > '1' && cmd0 < '7' = playFromTable game cmd1

        | otherwise = do putStrLn $ "Invalid command: " ++ command
                         return $ Game fg cg vg wg dg

        where cmd0 = head command
              cmd1 = if length command > 1 then head $ tail command else ' '

updateLoop :: Game -> IO ()
updateLoop game = 
    Control.Monad.unless (gameOver game) $ do
        command <- getLine
        updatedGame <- updateGame game command
        updateLoop updatedGame

