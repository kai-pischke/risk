module GameElements
    ( Player(..)
    ) where

data Player = Black | Blue | Green | Red | Yellow 
            deriving (Eq, Show, Ord)