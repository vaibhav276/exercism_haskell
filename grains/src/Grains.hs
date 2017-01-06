module Grains (square, total) where

import Data.Bits

square :: Integer -> Maybe Integer
square n 
    | n > 0 && n < 65 = Just $ (1 `shiftL` (n'-1)) 
    | otherwise       = Nothing
        where n' = fromIntegral n

total :: Integer
total = case (traverse square [1..64]) of
    Just xs -> sum xs
    Nothing -> 0
