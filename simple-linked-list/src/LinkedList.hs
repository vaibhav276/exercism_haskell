module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Node { datum :: a, next :: LinkedList a } 
                    | NullNode

fromList :: [a] -> LinkedList a
fromList [] = NullNode
fromList xs = Node (head xs) (fromList (tail xs))

isNil :: LinkedList a -> Bool
isNil NullNode = True
isNil _        = False

new :: a -> LinkedList a -> LinkedList a
new = Node

nil :: LinkedList a
nil = NullNode

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList NullNode   = []
toList (Node a l) = [a] ++ (toList l)
