module GameElements
    ( Player(..),
      Card(..),
      CardSet,
      TradeIn(..)
    ) where

data Player = Black | Blue | Green | Red | Yellow
            deriving (Eq, Show, Ord, Read)

data Card = Infantry | Cavalry | Artillery | Wild
            deriving (Eq, Show, Ord)

type CardSet = (Card, Card, Card)

data TradeIn = None | OneSet CardSet | TwoSet CardSet CardSet
            deriving (Eq, Show)