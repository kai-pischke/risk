module GameElements
    ( Player(..)
    ) where

data Player = Black | Blue | Green | Red | Yellow
            deriving (Eq, Show, Ord, Read)

data Card = Infantry | Cavalry | Artillery | Wild
            deriving (Eq, Show, Ord)
