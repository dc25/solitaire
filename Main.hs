import Data.Char
import Control.Monad
import Shuffle
import Card
import Game

-------------------------------------------------------------
-- The following routines do semantic checking and then
-- carry out a command.  By semantic checking I mean: make 
-- sure the command makes sense given the current game state.
-------------------------------------------------------------

-- Helper function
printAndReturn :: Game -> IO Game
printAndReturn game = do
    print game
    return game


playColumnToFoundation :: Game -> Int -> Int -> IO Game
playColumnToFoundation game@(Game fg cg _ _) index0 index1 
                | null.visible $ cg !! index0 = do
                    putStrLn $ "Can not move cards from empty column: " ++ show (index0+1)
                    return game

                | not $ (head.visible $ cg !! index0) `goesOnFoundation` (fg !! index1) = do
                    putStrLn $ "Can not move card from column: " ++ show (index0+1) ++ " to foundation: " ++ [chr (ord 'A' + index1)]
                    return game
                  
                | otherwise = printAndReturn $ fromColumnToFoundation game index0 index1

playColumnToColumn :: Game -> Int -> Int -> IO Game
playColumnToColumn game@(Game _ cg _ _) index0 index1 
                | index0 == index1 = do
                    putStrLn "Can not move cards from a column to itself."
                    return game

                | null.visible $ cg !! index0 = do
                    putStrLn $ "Can not move cards from empty column: " ++ show (index0+1)
                    return game

                | not $ (last.visible $ cg !! index0) `goesOnColumn` (cg !! index1) = do
                    putStrLn $ "Can not move cards from column: " ++ show (index0+1) ++ " to column: " ++ show (index1+1)
                    return game
                  
                | otherwise = printAndReturn $ fromColumnToColumn game index0 index1

playDeckToColumn :: Game -> Int -> IO Game
playDeckToColumn game@(Game _ cg dg _) index1 
                | null dg = do
                    putStrLn "Can not play from empty deck." 
                    return game

                | not $ head dg `goesOnColumn` (cg !! index1) = do
                    putStrLn $ "Can not move cards from deck to column: " ++ show (index1+1)
                    return game
                  
                | otherwise = printAndReturn $ fromDeckToColumn game index1 

playDeckToFoundation :: Game -> Int -> IO Game
playDeckToFoundation game@(Game fg _ dg _) index1 
                | null dg = do
                    putStrLn "Can not play from empty deck." 
                    return game

                | not $ head dg `goesOnFoundation` (fg !! index1) = do
                    putStrLn $ "Can not move cards from deck to foundation: " ++ [chr (ord 'A' + index1)]
                    return game
                  
                | otherwise = printAndReturn $ fromDeckToFoundation game index1 

playFromTable :: Game -> Char -> Char -> IO Game
playFromTable game cmd0 cmd1 
          | cmd1 >= '1' && cmd1 <= '7' = 
              playColumnToColumn game (ord cmd0 - ord '1') (ord cmd1 - ord '1') 
          | cmd1 >= 'A' && cmd1 <= 'D' = 
              playColumnToFoundation game (ord cmd0 - ord '1') (ord cmd1 - ord 'A') 
          | otherwise = do
              putStrLn "Invalid command." 
              return game
  
playFromDeck :: Game -> Char -> IO Game
playFromDeck game cmd1 
          | cmd1 >= '1' && cmd1 <= '7' =
              playDeckToColumn game (ord cmd1 - ord '1') 
          | cmd1 >= 'A' && cmd1 <= 'D' = 
              playDeckToFoundation game (ord cmd1 - ord 'A') 
          | otherwise = do
              putStrLn "Invalid command." 
              return game

updateGame :: Game -> String -> IO Game
updateGame game@(Game _ _ _ rg)  command
        | cmd0 == 'D' = if null rg then do 
                            putStrLn "No cards are available to Draw from.  Use 'R' to replenish."
                            return game
                        else 
                            printAndReturn $ fromReservesToDeck game

        | cmd0 == 'R' = if not (null rg) then do 
                            putStrLn "Can not Replenish reserves while it still contains cards."
                            return game
                        else 
                            printAndReturn $ fromDeckToReserves game

        | cmd0 == 'P' = playFromDeck game cmd1

        | cmd0 >= '1' && cmd0 <= '7' = playFromTable game cmd0 cmd1

        | otherwise = do putStrLn $ "Invalid command: " ++ command
                         return game

        where cmd0 = head command
              cmd1 = if length command > 1 then head $ tail command else ' '

updateLoop :: Game -> IO ()
updateLoop game = 
    Control.Monad.unless (gameOver game) $ do
        command <- getLine
        updatedGame <- updateGame game command
        updateLoop updatedGame

main :: IO ()
main = do 
          shuffledDeck <- shuffle [ Card r s | r<-[Ace .. King], s<-[Hearts .. Clubs]] 

          let
              foundations' = replicate 4 []
              columns' =     replicate 7 $ Column [] []
              deck' =       []
              game = Game foundations' columns' deck' shuffledDeck
              gameInPlay = start game

          print gameInPlay
          updateLoop gameInPlay
          putStrLn "Game Over"

