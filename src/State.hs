{-|
Module      : State
Description : Game representation.
Maintainer  : Kai

The basic representation of a Game excluding all dynamic and non-deterministic elements. 
This module focuses only on encapsulating all information needed to reconstruct a given game position
and doesn't focus on the actual mechanics or logic of gameplay.
-}
module State
    ( GameState,
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
      nextPhase,
      changeMiniPhase,
      cards,
      useCard,
      drawCard,
      kick,
      hasDrawn
    ) where

-- imports --
import System.Random
import Data.Map (Map)
import qualified Data.Map as Map
import RiskBoard
import Battle
import GameElements

--- internal representation of game state ---
data GameState = InternalGameState
   { troopMap :: Map Country Int,
     playerMap :: Map Country Player,
     stateStdGen :: StdGen,
     statePhase :: Phase,
     statePayers :: [Player],
     getsCard :: Bool,
     discardPile :: [Card],
     deck :: [Card],
     hand :: Map Player [Card] 
   } deriving (Eq, Show)
----------------------------------------------

-- types --
data MiniPhase = MidBattle 
                 Country    -- ^ Country from which attack is taking place
                 Country    -- ^ Country being attacked
                 Attackers  -- ^ Number of attacking troops
               | WonBattle
                 Country 
                 Country 
                 Attackers 
               | TimeToTrade
               | Normal 
               deriving (Eq, Show)
data Phase = Reinforce | Attack MiniPhase | Fortify
               deriving (Eq, Show)

-- helper functions for phases --
advancePhase :: Phase -> Phase
advancePhase Reinforce = Attack Normal
advancePhase (Attack _) = Fortify
advancePhase Fortify = Reinforce

updateMiniPhase :: MiniPhase -> Phase -> Phase
updateMiniPhase m (Attack _) = Attack m
updateMiniPhase _ y = y

-- general helper functions --
fmaybe :: a -> Maybe a -> a
fmaybe d = maybe d id 

rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

removeOneWithError :: Eq a => a -> String -> [a] -> [a]
removeOneWithError _ s [] = error s
removeOneWithError a s (x:xs) = if x == a then xs else x : removeOneWithError a s xs

modifyMapWithError :: Ord a => a -> (b -> b) -> String -> Map a b -> Map a b 
modifyMapWithError k f s m = Map.insert k (f v) m 
   where v = fmaybe (error s) (Map.lookup k m)

-- map functions for Internal State --
changeTroopMap :: (Map Country Int -> Map Country Int) -> GameState -> GameState
changeTroopMap f (InternalGameState t p g h l g' d d' h') = InternalGameState (f t) p g h l g' d d' h'

changePlayerMap :: (Map Country Player -> Map Country Player) -> GameState -> GameState
changePlayerMap f (InternalGameState t p g h l g' d d' h') = InternalGameState t (f p) g h l  g' d d' h'

changeGen :: (StdGen -> StdGen) -> GameState -> GameState
changeGen f (InternalGameState t p g h l g' d d' h') = InternalGameState t p (f g) h l g' d d' h'

changePhase :: (Phase -> Phase) -> GameState -> GameState
changePhase f (InternalGameState t p g h l g' d d' h') = InternalGameState t p g (f h) l g' d d' h'

changePlayer :: ([Player] -> [Player]) -> GameState -> GameState
changePlayer f (InternalGameState t p g h l g' d d' h') = InternalGameState t p g h (f l) g' d d' h'

-- changeGetCard :: (Bool -> Bool) -> GameState -> GameState
-- changeGetCard f (InternalGameState t p g h l g' d d' h') = InternalGameState t p g h l (f g') d d' h'

changeDisc :: ([Card] -> [Card]) -> GameState -> GameState
changeDisc f (InternalGameState t p g h l g' d d' h') = InternalGameState t p g h l g' (f d) d' h'

changeDeck :: ([Card] -> [Card]) -> GameState -> GameState
changeDeck f (InternalGameState t p g h l g' d d' h') = InternalGameState t p g h l g' d (f d') h'

changeHand :: (Map Player [Card] -> Map Player [Card]) -> GameState -> GameState
changeHand f (InternalGameState t p g h l g' d d' h') = InternalGameState t p g h l g' d d' (f h')

-- publicly exposed functions --

-- |Creates a blank game with no troops in any country. 
-- Players should be given in turn order starting with the current player.
newGame :: [Player] -> (Country -> (Player, Int))-> StdGen -> GameState
newGame [] _ _ = error "empty list (can't create game with no players)"
newGame listOfPlayer countryFunc startingStdGen = InternalGameState
   (Map.fromList $ zip countries $ map (snd . countryFunc) countries)
   (Map.fromList $ zip countries $ map (fst . countryFunc) countries)
   startingStdGen
   Reinforce
   listOfPlayer
   False
   (Wild : Wild : concat (map (replicate 14) [Infantry, Cavalry, Artillery]))
   []
   (Map.fromList $ zip listOfPlayer $ repeat [])
   where countries = [(minBound :: Country)..]

-- |Gives the number of troops in a given country.
troops :: GameState -> Country -> Int
troops g c = troopMap g Map.! c

-- |Returns a list of players (in the order of play) starting with the current player.
turnOrder :: GameState -> [Player]
turnOrder = statePayers

-- |Gives the player who owns a certain country.
owner :: GameState -> Country -> Player
owner g c = playerMap g Map.! c

-- |Modifies (adds or subtracts) the given number of troops (to or from a country).
changeTroops :: Country -> Int -> GameState -> GameState
changeTroops c i = changeTroopMap (Map.insertWith (+) c i)

-- |Replaces the owner of a country.
changeOwner :: Country -> Player -> GameState -> GameState
changeOwner c p = changePlayerMap (Map.insert c p)

-- |Advances to the next turn (and updates the phase).
nextTurn :: GameState -> GameState
nextTurn = changePlayer rotate . changePhase (const Reinforce)

-- |Gets the current StdGen.
currentStdGen :: GameState -> StdGen
currentStdGen = stateStdGen

-- |Replaces the current StdGen.
updateStdGen :: StdGen -> GameState -> GameState
updateStdGen = changeGen . const

-- |Gets the current 'Phase'.
phase :: GameState -> Phase
phase = statePhase

-- |Updates the current 'Phase'.
nextPhase :: GameState -> GameState
nextPhase = changePhase advancePhase

-- |Does nothing if not in 'Attack' 'Phase', otherwise sets phase to 'Attack' 'MiniPhase' 
-- (inserting the provided 'MiniPhase').
changeMiniPhase :: MiniPhase -> GameState -> GameState
changeMiniPhase = changePhase . updateMiniPhase

-- |Returns the list of cards in the player's hand
cards :: GameState -> Player -> [Card]
cards g p = fmaybe errP $ Map.lookup p (hand g)
   where errP = error $ "Player '" ++ show p ++ "' isn't in the game."

-- |Puts the card in the discard pile.
useCard :: Player -> Card -> GameState -> GameState
useCard p c = addToDiscard . removeCard
   where
   removeCard = changeHand $ modifyMapWithError p (removeOneWithError c errC) errP
   errP = "Player '" ++ show p ++ "' isn't in the game so can't use a card." 
   errC = "Player doesn't have card " ++ show c ++ "."
   addToDiscard = changeDisc (c:)

-- |Adds the top card to the player's hand. 
-- Shuffles the discard pile if necessary using a provided shuffle function.
drawCard :: Player -> ([Card] -> [Card]) -> GameState -> GameState
drawCard p shuff = takeTop . fillDeck
   where 
   fillDeck :: GameState -> GameState 
   fillDeck g = if null (deck g)
                then changeDeck (const $ shuff $ discardPile g) $ changeDisc (const []) g
                else g
   takeTop :: GameState -> GameState
   takeTop g = let c = head (deck g)
               in changeDeck tail $ changeHand (modifyMapWithError p (c:) errP) g
   errP = "Player '" ++ show p ++ "' isn't in the game so can't take a card." 

-- |Removes the 'Player' given in the second argument from the game.
-- Gives all cards to the 'Player' given in the first argument.
kick :: Player -> Player -> GameState -> GameState
kick p1 p2 = updateCards . removePlayer
   where 
   removePlayer = changePlayer (removeOneWithError p2 errP2)
   moveCards m = let cs = (m Map.! p2) 
                 in modifyMapWithError p1 (cs++) errP1 $ Map.delete p2 m
   updateCards = changeHand moveCards
   errP2 = "Can't kick '" ++ show p2 ++ "' since they aren't in the game."
   errP1 = show p2  ++ " can't be kick by " 
         ++ show p1 ++ " since " 
         ++ show p1 ++ " isn't in the game."

-- |@True@ if and only if the current player has drawn a card this turn.
hasDrawn :: GameState -> Bool
hasDrawn = getsCard