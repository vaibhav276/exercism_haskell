module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import Data.Map (Map, fromList, findWithDefault)
import Data.List (sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

students = ["Alice"  ,
            "Bob"    ,
            "Charlie",
            "David"  ,
            "Eve"    ,
            "Fred"   ,
            "Ginny"  ,
            "Harriet",
            "Ileana" ,
            "Joseph" ,
            "Kincaid",
            "Larry"  ]

defaultGarden :: String -> Map String [Plant]
defaultGarden = garden students

garden :: [String] -> String -> Map String [Plant]
garden ns = fromList . zip (sort ns) . map toPlants . plantStrings

lookupPlants :: String -> Map String [Plant] -> [Plant]
lookupPlants n m = findWithDefault [] n m

chunks2 :: [a] -> [[a]]
chunks2 (x1:x2:xs) = [[x1,x2]] ++ chunks2 xs
chunks2 _ = []

merge :: (Monoid a) => [a] -> [a] -> [a]
merge = zipWith mappend

plantStrings :: String -> [String]
plantStrings xs = merge as bs
    where (as:bs:_) = map chunks2 ( lines xs )

toPlants :: String -> [Plant]
toPlants xs = map toPlant xs
    where 
        toPlant :: Char -> Plant
        toPlant 'C' = Clover
        toPlant 'G' = Grass
        toPlant 'R' = Radishes
        toPlant 'V' = Violets

