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
import qualified Data.Map as Map
import RiskBoard
import Battle

data GameState = Undefined 
               deriving (Eq)
data Player = Black | Blue | Green | Red | Yellow 
               deriving (Eq, Show)
data MiniPhase = WonBattle Country Country Attackers | Normal 
               deriving (Eq, Show)
data Phase = Reinforce | Attack MiniPhase | Fortify
               deriving (Eq, Show)

newGame :: [Player] -> StdGen -> GameState
newGame = undefined

troops :: GameState -> Country -> Int
troops = undefined

turnOrder :: GameState -> [Player]
turnOrder = undefined

owner :: GameState -> Country -> Player
owner = undefined

changeTroops :: Country -> Int -> GameState -> GameState
changeTroops = undefined

changeOwner :: Country -> Player -> GameState -> GameState
changeOwner = undefined

nextTurn :: GameState -> GameState
nextTurn = undefined

currentStdGen :: GameState -> StdGen
currentStdGen = undefined

updateStdGen :: StdGen -> GameState -> GameState
updateStdGen = undefined

phase :: GameState -> Phase
phase = undefined

nextPhase :: GameState -> GameState
nextPhase = undefined