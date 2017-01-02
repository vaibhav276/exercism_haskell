module Bob (responseFor) where

import Data.Char (isAlpha, isUpper)

responseFor :: String -> String
responseFor xs
    | isSilence xs = "Fine. Be that way!"
    | isYell xs = "Whoa, chill out!"
    | isQuestion xs = "Sure."
    | otherwise = "Whatever."

-- | isSilence - whether string is nothing but spaces
isSilence :: String -> Bool
isSilence xs = (length . strip) xs == 0

-- | isYell - whether given string contains all upper case alphabets (wherever
-- there are alphabets)
isYell :: String -> Bool
isYell = (all' isUpper) . (filter isAlpha) . strip

-- | isQuestion - whether string ends with a '?'
isQuestion :: String -> Bool
isQuestion xs = (last . strip) xs == '?'

-- | strip just removes extra spaces from a string
strip :: String -> String
strip = unwords . words

-- | all' is a stricter variation of all, which returns False for empty arrays
all' :: (a -> Bool) -> [a] -> Bool
all' f [] = False
all' f xs = all f xs
