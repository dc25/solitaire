module Shuffle
( shuffle 
) where

import Haste.App 

shuffle :: [a] -> IO [a]
shuffle ys = do
    gen <- newSeed 
    return $ shuffle' gen ys where
        shuffle' _ [] = []
        shuffle' gen ys' = 
            let (r, newGen) = randomR (0, length ys' ) gen
                (a,b) = splitAt r ys'
            in head b : shuffle' newGen (a ++ tail b)

