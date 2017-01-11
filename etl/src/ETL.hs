module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)
import Data.List (union)

transform :: (Eq a) => Map a String -> Map Char a
transform = fromList 
    . map toLowerTuple
    . foldr union [] 
    . map expandTuple
    . map swapTuple 
    . toList

swapTuple :: (a,b) -> (b,a)
swapTuple (a,b) = (b,a)

expandTuple :: ([a],b) -> [(a,b)]
expandTuple (xs,y) = [ (x,y) | x <- xs ]

toLowerTuple :: (Char,a) -> (Char,a)
toLowerTuple (x,y) = (toLower x, y)
