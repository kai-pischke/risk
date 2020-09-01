{-|
Module      : GameElements
Description : Game pieces.
Maintainer  : Kai, Alex, River

This module holds the types that represent individual game pieces and other game-specific enumerated types.
-}
module GameElements
    ( Player(..),
      Card(..),
      CardSet,
      TradeIn(..)
    ) where

-- | Each player is represented by the colour they play as.
data Player = Black | Blue | Green | Red | Yellow
            deriving (Eq, Show, Ord, Read)

-- | We ignore the country shown on the card.
data Card = Infantry | Cavalry | Artillery | Wild
            deriving (Eq, Show, Ord, Read)

-- | Cards always come in sets of 3 when being traded in.
type CardSet = (Card, Card, Card)

-- | A player csan trade in at most 2 sets at once.
data TradeIn = None | OneSet CardSet | TwoSet CardSet CardSet
            deriving (Eq, Show)
