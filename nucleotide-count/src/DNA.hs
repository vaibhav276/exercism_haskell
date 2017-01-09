module DNA (count, nucleotideCounts) where

import Data.Map (Map, lookup, fromList, adjust, member)
import Control.Monad (foldM)

initMap :: Map Char Int
initMap = fromList [('A',0),('G',0),('T',0),('C',0)]

strInvalidNucleotide :: String
strInvalidNucleotide = "Invalid Nucleotide"

count :: Char -> String -> Either String Int
count c xs = do
    m <- nucleotideCounts xs 
    maybeToEither strInvalidNucleotide (Data.Map.lookup c m)

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts = foldM mapStep initMap

mapStep :: (Map Char Int) -> Char -> Either String (Map Char Int)
mapStep m c = case member c initMap of
    True  -> Right $ adjust succ c m
    False -> Left strInvalidNucleotide

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither s x = case x of
    Just y  -> Right y
    Nothing -> Left s
