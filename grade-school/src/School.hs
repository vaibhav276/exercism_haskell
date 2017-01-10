module School (School, add, empty, grade, sorted) where

import Data.Map (Map, fromList, toList, mapWithKey, adjust, member, insert, 
                singleton, insertWith, findWithDefault)
import qualified Data.Map as M (empty, lookup)
import Data.List (sort)

-- data School = Dummy
type Grade  = Int
type Name   = String
type School = Map Grade [Name]

add :: Grade -> Name -> School -> School
add n x = insertWith (++) n [x]

empty :: School
empty = M.empty

grade :: Grade -> School -> [Name]
grade n = sort . findWithDefault [] n 

sorted :: School -> [(Grade, [Name])]
sorted = toList . mapWithKey sort' where sort' k = sort
