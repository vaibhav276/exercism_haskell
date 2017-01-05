module Squares (difference, squareOfSums, sumOfSquares) where

difference :: Integral a => a -> a
difference = (-) <$> squareOfSums <*> sumOfSquares

squareOfSums :: Integral a => a -> a
squareOfSums n = (round (n' * (n' + 1) / 2)) ^ 2
    where n' = fromIntegral n

sumOfSquares :: Integral a => a -> a
sumOfSquares n = round (n' * (n' + 1) * (2*n' + 1) / 6)
    where n' = fromIntegral n
