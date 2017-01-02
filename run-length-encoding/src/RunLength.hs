module RunLength (decode, encode) where

import Data.Char (isDigit)

decode :: String -> String
decode [] = []
decode xs = (replicate n c) ++ decode xs'
    where
        n   | isDigit . head $ xs = read . takeWhile isDigit $ xs
            | otherwise           = 1
        c                         = head . dropWhile isDigit $ xs
        xs' | isDigit . head $ xs = tail . dropWhile isDigit $ xs 
            | otherwise           = tail xs

encode :: String -> String
encode [] = []
encode xs = (str n) ++ [c] ++ encode xs'
    where
        n      = length . takeWhile (==head xs) $ xs
        c      = head xs
        xs'    = dropWhile (==head xs) $ xs
        str 1  = ""
        str n  = show n
