module Shuffle
( shuffle 
) where

import Haste.App 

-- Note that randomR has higher bound of "length ys'-1"
-- According to the documentation, the higher bound is
-- "exclusive".  If this is true then the higher bound
-- should be "length ys'" .  However, setting the bound 
-- to "length ys'" causes bad runtime behavior so I am
-- guessing that the bound is "inclusive".  
--
-- How can I verify that this is the case other than
-- just observing the result.  Would like to be able
-- to do an assert().
--

shuffle :: [a] -> IO [a]
shuffle ys = do
    gen <- newSeed 
    return $ shuffle' gen ys where
        shuffle' _ [] = []
        shuffle' gen ys' = 
            let (r, newGen) = randomR (0, length ys'-1 ) gen -- see comment
                (a,b) = splitAt r ys'
            in head b : shuffle' newGen (a ++ tail b)

