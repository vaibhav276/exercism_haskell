module Hamming (distance) where

distance :: (Eq a, Num b) => [a] -> [a] -> Maybe b
distance (x:xs) (y:ys) = (+) <$> Just (point x y) <*> distance xs ys
    where point x y | x == y = 0 | otherwise = 1
distance [] [] = Just 0
distance _  _  = Nothing
