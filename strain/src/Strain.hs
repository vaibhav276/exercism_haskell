module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard f = keep (not . f)

keep :: (a -> Bool) -> [a] -> [a]
keep f (x:xs)
    | f x       = x : keep f xs
    | otherwise = keep f xs
keep _ _ = []
