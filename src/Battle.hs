{-|
Module      : Battle
Description : Battle interaction logic.
Maintainer  : Kai

This module deals with attacking and defending types and gives an interface to calculate battle outcomes.
-}
module Battle 
    ( Defenders(..),
      Attackers(..),
      doBattle,
    ) where
    
import System.Random
import Data.List
data Defenders = OneDef | TwoDef deriving (Eq, Show, Ord)
data Attackers = OneAtt | TwoAtt | ThreeAtt deriving (Eq, Show, Ord)

-- | Note that for Enum, enumeration starts at 1 not 0.
instance Enum Defenders where
    fromEnum OneDef = 1
    fromEnum TwoDef = 2 
    toEnum 1 = OneDef
    toEnum 2 = TwoDef
    toEnum n = error ("toEnum{Defenders}: tag (" ++ show n ++ ") is outside of enumeration's range (1,2)")

-- | Note that for Enum, enumeration starts at 1 not 0.
instance Enum Attackers where
    fromEnum OneAtt = 1
    fromEnum TwoAtt = 2
    fromEnum ThreeAtt = 3 
    toEnum 1 = OneAtt
    toEnum 2 = TwoAtt
    toEnum 3 = ThreeAtt
    toEnum n = error ("toEnum{Attackers}: tag (" ++ show n ++ ") is outside of enumeration's range (1,3)")

roll :: RandomGen g => Int -> g -> (g, [Int])
roll 0 g = (g, [])
roll n g = let (x, g') = uniformR (1, 6) g
           in (x:) <$> roll (n-1) g'

antisort :: Ord a => [a] -> [a]
antisort = reverse . sort

-- | Takes a number of attackers ('Attackers') and a number of defenders ('Defenders') and calculates the attackers lost, the defeners lost and the new StdGen (returned in a tuple in that order).
doBattle :: Attackers -> Defenders -> StdGen -> (Int, Int, StdGen)
doBattle a d g = (attackerLosses, defenderLosses, g'')
  where
    (g', as)  = roll (fromEnum a) g
    (g'', ds) = roll (fromEnum d) g'
    result = zip (antisort as) (antisort ds) 
    attackerLosses = length $ filter (uncurry (<=)) result
    defenderLosses = length $ filter (uncurry (>)) result