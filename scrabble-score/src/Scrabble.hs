module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)
import qualified Data.Map as M (Map, fromList, findWithDefault)

weights :: M.Map Char Int
weights = M.fromList $ ['A'..'Z'] `zip` (score <$> ['A'..'Z'])
    where
        score a
            | a `elem` "AEIOULNRST" = 1
            | a `elem` "DG" = 2
            | a `elem` "BCMP" = 3
            | a `elem` "FHVWY" = 4
            | a `elem` "K" = 5
            | a `elem` "JX" = 8
            | a `elem` "QZ" = 10
            | otherwise = 0

scoreLetter :: Char -> Int
scoreLetter x = M.findWithDefault 0 (toUpper x) weights

scoreWord :: String -> Int
scoreWord = sum . fmap scoreLetter
