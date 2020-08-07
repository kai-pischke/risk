module SetupBoard
        ( SetupState(..),
          emptyBoard,
          placeTroop,
          completeBoardOwner
        ) where

import Data.Map (Map)
import Data.Maybe
import Control.Monad (join)
import qualified Data.Map as Map
import RiskBoard
import Battle
import GameElements

data SetupBoardState = InternalGameState
                { troopMap :: Map Country Int,
                    playerMap :: Map Country (Maybe Player),
                    statePlayers :: [Player],
                    playerRemaining ::  Map Player Int
                } deriving (Eq, Show)

data SetupState = Incomplete SetupBoardState
                | PartiallyComplete SetupBoardState
                | Complete SetupBoardState
                deriving (Eq, Show)
                
-- map functions --
type SbStateTransformer = SetupBoardState -> SetupBoardState

changeTroopMap :: (Map Country Int -> Map Country Int) -> SbStateTransformer
changeTroopMap f (InternalGameState tm pm sp pr) = InternalGameState (f tm) pm sp pr

changePlayerMap :: (Map Country (Maybe Player) -> Map Country (Maybe Player)) -> SbStateTransformer
changePlayerMap f (InternalGameState tm pm sp pr) = InternalGameState tm (f pm) sp pr

changePlayers :: ([Player] -> [Player]) -> SbStateTransformer
changePlayers f (InternalGameState tm pm sp pr) = InternalGameState tm pm (f sp) pr

changeRemaining :: (Map Player Int -> Map Player Int) -> SbStateTransformer
changeRemaining f (InternalGameState tm pm sp pr) = InternalGameState tm pm sp (f pr)

-- helper functions --
currentPlayer :: SetupBoardState -> Player
currentPlayer = head . statePlayers

toSetupState :: SetupBoardState -> SetupState
toSetupState s | not $ null $ Map.filter isNothing $ playerMap s    = Incomplete s
               | not $ null $ Map.filter (/= 0) $ playerRemaining s = PartiallyComplete s
               | otherwise                                          = Complete s
              
nextTurn :: SetupBoardState -> SetupBoardState
nextTurn = changePlayers rotate
        where
          rotate [] = []
          rotate (x:xs) = xs ++ [x]

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = (not $ x `elem` xs) && distinct xs

-- public --
emptyBoard :: [Player] -> SetupState
emptyBoard ps = Incomplete $ InternalGameState
    (Map.fromList $ zip countries $ repeat 0)
    (Map.fromList $ zip countries $ repeat Nothing)
    ps
    (Map.fromList $ zip ps $ repeat $ initialTroops $ length ps)
    where
        countries = [(minBound :: Country)..]



placeTroop :: Country -> SetupState -> Maybe SetupState
placeTroop c (Incomplete s) = case Map.lookup c (playerMap s) of
    Just Nothing -> if valid then Just $ toSetupState $ newBoard else Nothing
    _            -> Nothing
    where
        p = currentPlayer s
        valid = maybe (error $ "Player '" ++ show p ++ "' not playing.")
                      (>0) (Map.lookup p $ playerRemaining s)
        ownerAdded = changePlayerMap (Map.insert c (Just p)) s
        decremented = changeRemaining (Map.insertWith (flip (-)) p 1) ownerAdded
        troopAdded = changeTroopMap (Map.insertWith (+) c 1) decremented
        newBoard = nextTurn $ troopAdded


placeTroop c (PartiallyComplete s) = case Map.lookup c (playerMap s) of
     (Just (Just x)) -> if valid && x == p
                        then Just $ toSetupState $ newBoard
                        else Nothing
     _               -> error "impossible situation: Nothing in PartiallyComplete"
    where
        p = currentPlayer s
        valid = maybe (error $ "Player '" ++ show p ++ "' not playing.")
                                    (>0) (Map.lookup p $ playerRemaining s)
        decremented = changeRemaining (Map.insertWith (flip (-)) p 1) s
        troopAdded = changeTroopMap (Map.insertWith (+) c 1) decremented
        newBoard = nextTurn $ troopAdded

placeTroop _ _ = error "invalid call to placeTroop: called on 'Complete' SetupState."

completeBoardOwner :: SetupState -> Country -> (Player, Int)
completeBoardOwner (Complete s) c = result
        where Just result = (,) <$> (join $ Map.lookup c $ playerMap s) <*> (Map.lookup c $ troopMap s)
completeBoardOwner _ _ = error "completeBoardOwner only defined for Complete SetupBoardState"

-- private --
initialTroops :: Int -> Int
initialTroops 6 = 20
initialTroops 5 = 25
initialTroops 4 = 30
initialTroops 3 = 35
initialTroops 2 = 40
initialTroops n = error $ "Game rules only specified for 2-6 players. Not sure what to do for "
                        ++ show n ++ " players."
