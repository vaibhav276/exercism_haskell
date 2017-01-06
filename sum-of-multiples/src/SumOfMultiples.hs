module SumOfMultiples (sumOfMultiples) where

import Data.List (union)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples xs l = sum . foldr union [] . map (factors l) $ xs
    where factors l x = [x, (2*x) .. (l - 1)]
