module State
    ( GameState,
      Player(..),
      MiniPhase(..),
      Phase(..),
      newGame,
      troops,
      turnOrder,
      owner,
      changeTroops,
      changeOwner,
      nextTurn,
      currentStdGen,
      updateStdGen,
      phase,
      nextPhase
    ) where

import System.Random
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import RiskBoard
import Battle

-- this is stupid, I will find a way to get rid of this.
instance Eq StdGen where 
   a == b = show a == show b

-- internal representation of game state
data GameState = InternalGameState
   { troopMap :: Map Country Int,
     stateStdGen :: StdGen,
     statePhase :: Phase,
     statePayers :: [Player]
   } deriving (Eq)



data Player = Black | Blue | Green | Red | Yellow 
               deriving (Eq, Show)
data MiniPhase = WonBattle Country Country Attackers | Normal 
               deriving (Eq, Show)
data Phase = Reinforce | Attack MiniPhase | Fortify
               deriving (Eq, Show)

advancePhase :: Phase -> Phase 
advancePhase Reinforce = Attack Normal
advancePhase Attack _ = Fortify
advancePhase Fortify = Reinforce

-- map functions for Internal State
changeMap :: (Map Country Int -> Map Country Int) -> GameState -> GameState
changeMap f (InternalGameState t g h l) = InternalGameState (f t) g h l
   
changeGen :: (StdGen -> StdGen) -> GameState -> GameState
changeGen f (InternalGameState t g h l) = InternalGameState t (f g) h l
   
changePhase :: (Phase -> Phase) -> GameState -> GameState
changePhase f (InternalGameState t g h l) = InternalGameState t g (f h) l

changePlayer :: ([Player] -> [Player]) -> GameState -> GameState
changePlayer f (InternalGameState t g h l) = (InternalGameState t g (f h) l)


newGame :: [Player] -> StdGen -> GameState
newGame listOfPlayer startingStdGen = InternalGameState 
   (Map.fromList $ zip [(minBound :: Country)..] [0..])
   startingStdGen
   Reinforce
   listOfPlayer

troops :: GameState -> Country -> Int
troops g c = fromMaybe (error "you gave me an unexpected country") (Map.lookup c (troopMap g))

turnOrder :: GameState -> [Player]
turnOrder = statePayers

owner :: GameState -> Country -> Player
owner = troopMap

changeTroops :: Country -> Int -> GameState -> GameState
changeTroops = undefined

changeOwner :: Country -> Player -> GameState -> GameState
changeOwner = undefined

nextTurn :: GameState -> GameState
nextTurn = changePlayer rotate . changePhase (const Reinforce)
   where 
      rotate :: [a] -> [a]
      rotate [] = []
      rotate (x:xs) = xs ++ [x]

currentStdGen :: GameState -> StdGen
currentStdGen = stateStdGen

updateStdGen :: StdGen -> GameState -> GameState
updateStdGen = changeGen . const

phase :: GameState -> Phase
phase = statePhase

nextPhase :: GameState -> GameState
nextPhase = changePhase advancePhase