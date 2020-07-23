module Battle 
    ( Defenders(..),
      Attackers(..),
      doBattle
    ) where

import System.Random
data Defenders = OneDef | TwoDef deriving (Eq, Show, Ord)
data Attackers = OneAtt | TwoAtt | ThreeAtt deriving (Eq, Show, Ord)

instance Enum Defenders where
    fromEnum OneDef = 1
    fromEnum TwoDef = 2 
    toEnum 1 = OneDef
    toEnum 2 = TwoDef
    toEnum n = error ("toEnum{Defenders}: tag (" ++ show n ++ ") is outside of enumeration's range (1,2)")

instance Enum Attackers where
    fromEnum OneAtt = 1
    fromEnum TwoAtt = 2
    fromEnum ThreeAtt = 3 
    toEnum 1 = OneAtt
    toEnum 2 = TwoAtt
    toEnum 3 = ThreeAtt
    toEnum n = error ("toEnum{Attackers}: tag (" ++ show n ++ ") is outside of enumeration's range (1,3)")

doBattle :: Attackers -> Defenders -> StdGen -> (Int, Int, StdGen)
doBattle = undefined
