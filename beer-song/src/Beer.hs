module Beer (song) where

song :: String
song = concatMap line (reverse [0..99])

-- | line returns the nth line of the song. Only non-negative 'n' would make a
-- correct song line
line :: Int -> String
line 0 = "No more bottles of beer on the wall, no more bottles of beer.\n\
         \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
line n = bottle n ++ " of beer on the wall, " ++ bottle n ++ " of beer.\n\
         \Take " ++ it n ++ " down and pass it around, " ++ bottle (n - 1) ++ " of beer on the wall.\n\
         \\n"
         where
            bottle 0 = "no more bottles"
            bottle 1 = "1 bottle"
            bottle n = show n ++ " bottles"
            it 1 = "it"
            it _ = "one"
