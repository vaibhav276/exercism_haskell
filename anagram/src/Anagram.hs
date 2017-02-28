module Anagram (anagramsFor) where

import Data.Char (ord, toUpper)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor = filter . isAnagram

isAnagram :: String -> String -> Bool
isAnagram xs ys
    | length xs    /= length ys    = False
    | sumAscii xs' /= sumAscii ys' = False
    | xs'          == ys'          = False
    | sort xs'     /= sort ys'     = False
    | otherwise                    = True
        where
            xs' = map toUpper xs
            ys' = map toUpper ys
            sumAscii = sum . map ord
