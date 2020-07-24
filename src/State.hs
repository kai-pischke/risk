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
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import RiskBoard
import Battle

-- this is stupid, I will find a way to get rid of this.
instance Eq StdGen where 
   a == b = show a == show b

-- internal representation of game state
data GameState = InternalGameState
   { troopMap :: Map Country Int,
     playerMap :: Map Country Player,
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
advancePhase (Attack _) = Fortify
advancePhase Fortify = Reinforce

-- map functions for Internal State
changeTroopMap :: (Map Country Int -> Map Country Int) -> GameState -> GameState
changeTroopMap f (InternalGameState t p g h l) = InternalGameState (f t) p g h l

changePlayerMap :: (Map Country Player -> Map Country Player) -> GameState -> GameState
changePlayerMap f (InternalGameState t p g h l) = InternalGameState t (f p) g h l
   
changeGen :: (StdGen -> StdGen) -> GameState -> GameState
changeGen f (InternalGameState t p g h l) = InternalGameState t p (f g) h l
   
changePhase :: (Phase -> Phase) -> GameState -> GameState
changePhase f (InternalGameState t p g h l) = InternalGameState t p g (f h) l

changePlayer :: ([Player] -> [Player]) -> GameState -> GameState
changePlayer f (InternalGameState t p g h l) = InternalGameState t p g h (f l)
--

newGame :: [Player] -> (Country -> (Player, Int))-> StdGen -> GameState
newGame listOfPlayer countryFunc startingStdGen = InternalGameState 
   (Map.fromList $ zip countries $ map (snd . countryFunc) countries)
   (Map.fromList $ zip countries $ map (fst . countryFunc) countries)
   startingStdGen
   Reinforce
   listOfPlayer
   where countries = [(minBound :: Country)..]

troops :: GameState -> Country -> Int
troops g c = fromMaybe (error "unexpected country") (Map.lookup c (troopMap g))

turnOrder :: GameState -> [Player]
turnOrder = statePayers

owner :: GameState -> Country -> Player
owner g c = fromMaybe (error "unexpected country") (Map.lookup c (playerMap g))

changeTroops :: Country -> Int -> GameState -> GameState
changeTroops c i = changeTroopMap (Map.insertWith (+) c i)

changeOwner :: Country -> Player -> GameState -> GameState
changeOwner c p = changePlayerMap (Map.insert c p)

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
