module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

import Data.Foldable (foldl')

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot { bearing :: Bearing,
                     coordinates :: (Integer, Integer) }

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot -- the value constructor

simulate :: Robot -> String -> Robot
simulate = foldl' move

move :: Robot -> Char -> Robot
move r 'A' = Robot b (x', y') where 
    b       = bearing r
    (x',y') = advance b (coordinates r)
    advance North (x,y) = (x,y+1)
    advance West  (x,y) = (x-1,y)
    advance East  (x,y) = (x+1,y)
    advance South (x,y) = (x,y-1)
move r 'L' = Robot (turnLeft  (bearing r)) (coordinates r)
move r 'R' = Robot (turnRight (bearing r)) (coordinates r)
move r _   = r

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

turnRight :: Bearing -> Bearing
turnRight North = East
turnRight West  = North
turnRight South = West
turnRight East  = South
