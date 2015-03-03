game = 5

updateGame game command = game-1

gameOver game = game == 0

main = do 
          updateLoop game
          putStrLn "Game Over"

updateLoop game = 
    if gameOver game then 
        return 0
    else do
        command <- getLine
        putStrLn command
        updateLoop $ updateGame game command

