module SumOfMultiples (sumOfMultiples) where

import Data.List (union)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0
sumOfMultiples xs l = sum . foldr1 union . map (factors l) $ xs
    where factors l x = [x, (2*x) .. (l - 1)]
