module Phone (areaCode, number, prettyPrint) where

import Data.Char (isDigit)

areaCode :: String -> Maybe String
areaCode xs = take 3 <$> number xs

prefix :: String -> Maybe String
prefix xs = take 3 . drop 3 <$> number xs

line :: String -> Maybe String
line xs = take 4 . drop 6 <$> number xs

number :: String -> Maybe String
number xs
    | length xs' == 11 && head xs' == '1' = Just (tail xs')
    | length xs' == 10                    = Just xs'
    | otherwise                           = Nothing
        where
            xs' = filter (isDigit) xs

prettyPrint :: String -> Maybe String
prettyPrint xs = do
    a <- areaCode xs
    p <- prefix xs
    l <- line xs
    return ("(" ++ a ++ ") " ++ p ++ "-" ++ l)
