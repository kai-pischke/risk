module Battle 
    ( Defenders(..),
      Attackers(..),
      doBattle
    ) where

import System.Random

data Defenders = OneDef | TwoDef 
               deriving (Eq, Show, Ord, Enum) -- note: deriving Enum won't index correctly
data Attackers = OneAtt | TwoAtt | ThreeAtt
               deriving (Eq, Show, Ord, Enum)

doBattle :: Attackers -> Defenders -> StdGen -> (Int, Int, StdGen)
doBattle = undefined