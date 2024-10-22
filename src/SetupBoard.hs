{-|
Module      : SetupBoard
Description : Game representation during setup.
Maintainer  : Kai

The is a basic representation of the Game during the initial troop placement rounds.
-}
module SetupBoard
        ( SetupState(..),
          emptyBoard,
          placeTroop,
          completeBoardOwner,
          partiallyCompleteBoardOwner,
          incompleteBoardOwner,
          setUpTurnOrder
        ) where

import Data.Map (Map, fromList, assocs, (!?), (!))
import Data.Maybe (fromJust, isNothing)
import Control.Monad (join)
import Data.Aeson
import Data.Text (pack)
import Text.Read (readMaybe)

import qualified Data.Map as Map
import RiskBoard
import GameElements
import ParsePart

-- | Keeps track of how many troops on each country, who owns each country as well as player order and number of troops remaining to be placed for each player.
data SetupBoardState = InternalGameState
                { troopMap :: Map Country Int,
                    playerMap :: Map Country (Maybe Player),
                    statePlayers :: [Player],
                    playerRemaining ::  Map Player Int
                } deriving (Eq, Show)

-- | Contains a SetupBoardState and information about how complete the board is.
data SetupState = Incomplete SetupBoardState -- ^ when not all countries have a owner
                | PartiallyComplete SetupBoardState -- ^ when all countries have an owner but not all troops have been placed
                | Complete SetupBoardState -- ^ when all troops have been placed
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

-- | Creates a blank board with no troops and where no countries are owned yet. Creates an 'Incomplete' 'SetupBoardState'.
emptyBoard :: [Player] -> SetupState
emptyBoard ps = if distinct ps then
    Incomplete $ InternalGameState
    (Map.fromList $ zip countries $ repeat 0)
    (Map.fromList $ zip countries $ repeat Nothing)
    ps
    (Map.fromList $ zip ps $ repeat $ initialTroops $ length ps)
    else errDup
    where
        countries = [(minBound :: Country)..]
        errDup = error "Duplicate players given to emptyBoard"

-- | Partial function, only defined for incomplete and partially complete SetupState and only when the current player owns the given country.
-- It should error if called on a Complete SetupBoardState.
-- It should return 'Nothing' if called on a 'Country' not owned by current player (or unowned).
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
completeBoardOwner (Complete s) c = case result of
                                    (Just r) -> r
                                    Nothing  -> error "Shouldn't be possibe. Missing owner in CompleteBoard!"
        where
          result = (,) <$> (join $ Map.lookup c $ playerMap s) <*> (Map.lookup c $ troopMap s)
completeBoardOwner _ _ = error "completeBoardOwner only defined for Complete SetupBoardState"

-- | Partial function, defined for 'PartiallyComplete' 'SetupState', gives the owner and number of troops in each country.
-- It should error if called on a incomplete SetupState
partiallyCompleteBoardOwner :: SetupState -> Country -> (Player, Int)
partiallyCompleteBoardOwner (PartiallyComplete s) c = case result of
                                    (Just r) -> r
                                    Nothing  -> error "Shouldn't be possibe. Missing owner in PartiallyCompleteBoard!"
        where
          result = (,) <$> (join $ Map.lookup c $ playerMap s) <*> (Map.lookup c $ troopMap s)
partiallyCompleteBoardOwner _ _ = error "partiallyCompleteBoardOwner only defined for PartiallyComplete SetupBoardState"

-- | Partial function, only defined for any 'SetupState', gives the owner and number of troops in each country.
incompleteBoardOwner :: SetupState -> Country -> (Maybe Player, Int)
incompleteBoardOwner (Incomplete s) c = case result of
                                        (Just r) -> r
                                        Nothing  -> error "incompleteBoardOwner: Map should be total. Shouldn't be possible."
  where result = (,) <$> (Map.lookup c $ playerMap s) <*> (Map.lookup c $ troopMap s)
incompleteBoardOwner _ _ = error "incompleteBoardOwner only defined for Incomplete SetupBoardState"

-- | Gives the turnOrder for a 'SetupState', with the player's whose turn it currently is at the head of the list.
setUpTurnOrder :: SetupState -> [Player]
setUpTurnOrder = statePlayers . internalBoard

-- private --
initialTroops :: Int -> Int
initialTroops 6 = 20
initialTroops 5 = 25
initialTroops 4 = 30
initialTroops 3 = 35
initialTroops 2 = 40
initialTroops n = error $ "Game rules only specified for 2-6 players. Not sure what to do for "
                        ++ show n ++ " players."

internalBoard :: SetupState -> SetupBoardState
internalBoard (Incomplete s) = s
internalBoard (PartiallyComplete s) = s
internalBoard (Complete s) = s

instance ToJSON SetupState where
    toJSON ss = toJSON  $ internalBoard ss

instance FromJSON SetupState where
    parseJSON (Object v) = do
        sbs <- parseJSON (Object v)

        return (toSetupState sbs)
    parseJSON _ = mempty

instance ToJSON SetupBoardState where
    toJSON (InternalGameState troopsMap playersMap players remainingToPlace) =
        object [pack "kind" .= pack "Setup",
                pack "players" .= players,
                pack "playerMap" .= ((fromList.zip (map show countries)) $ map ((\mp -> if (mp == Nothing) then Unowned else Owner (fromJust mp)). (playersMap !)) countries),
                pack "troopMap" .= ((fromList.zip (map show countries)) $ map (troopsMap !) countries),
                pack "playerRemaining" .= (fromList.map (\p -> (show $ fst p, snd p)) .assocs) remainingToPlace]
        where
            countries = [(minBound :: Country)..]


instance FromJSON SetupBoardState where
    parseJSON (Object v) = do
        kind <- (v.: pack "kind")
        if (kind /= "Setup")
            then do mempty
            else do
                players <- (v.: pack "players")
                pMap <- (v.: pack "playerMap")
                tMap <- (v.: pack "troopMap")
                pRem <- (v.: pack "playerRemaining")

                let pListR = map (\c -> (c, pMap !? (show c))) countries
                let tListR = map (\c -> (c, tMap !? (show c))) countries
                let remListR = map (\p -> (readMaybe $ fst p, snd p)) $ assocs pRem

                if (any (\p -> snd p == Nothing) pListR || any (\p -> snd p == Nothing) tListR || any (\p -> fst p == Nothing) remListR)
                    then do mempty
                    else do

                        let playersMap = fromList $ map (\p -> (fst p, fromJust $ snd p)) pListR
                        let troopsMap = fromList $ map (\p -> (fst p, fromJust $ snd p)) tListR
                        let remMap = fromList $ map (\p -> (fromJust $ fst p, snd p)) remListR

                        return (InternalGameState troopsMap playersMap players remMap)
        where
            countries = [(minBound ::Country)..]

    parseJSON _ = mempty
