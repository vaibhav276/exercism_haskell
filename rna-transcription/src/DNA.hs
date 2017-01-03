module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA "C"    = Just "G"
toRNA "G"    = Just "C"
toRNA "T"    = Just "A"
toRNA "A"    = Just "U"
toRNA ([x])  = Nothing
toRNA (x:xs) = (++) <$> toRNA [x] <*> toRNA xs
