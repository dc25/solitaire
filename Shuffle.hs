module Shuffle
( shuffle 
) where

import System.Random

shuffle :: [a] -> IO [a]
shuffle ys = do
    gen <- newStdGen
    return $ shuffle' gen ys where
        shuffle' _ [] = []
        shuffle' gen ys' = 
            let (r, newGen) = randomR (0, length ys' - 1) gen
                (a,b) = splitAt r ys'
            in head b : shuffle' newGen (a ++ tail b)

