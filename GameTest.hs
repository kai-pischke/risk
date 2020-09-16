
data SetupBoardState = InternalGameState
                { troopMap :: Map Country Int,
                    playerMap :: Map Country (Maybe Player),
                    statePlayers :: [Player],
                    playerRemaining ::  Map Player Int
                } deriving (Eq, Show)


instance ToJSON SetupBoardState where
    toJSON (InternalGameState troopsMap, playersMap, players, remainingToPlace) =
        object [pack "kind" .= pack "Setup",
                pack "players" .= players,
                pack "playerTroopsMap" .= ((fromList.zip (map show countries)) $ map setupBoardOwner countries),
                pack "playerRemaining" .= (fromList.map (\p -> (show $ fst p, snd p)) .assocs) remainingToPlace]
        where
            countries = [(minBound :: Country)..]
            setupBoardOwner:: SetupState -> Country -> (Owner, String)
            setupBoardOwner (Incomplete s) c = if (p == Nothing) then (Unowned, n) else (fromJust p, show n)
                where
                    (p,n) = incompleteBoardOwner (Incomplete s) c
            setupBoardOwner (PartiallyComplete s)  c = (Owner p, show n)
                where
                    (p,n) = (partiallyCompleteBoardOwner (PartiallyComplete s) c)

            setupBoardOwner s c = (Owner p, show n)
                where
                    (p,n) = (completeBoardOwner s c)



instance FromJSON GameState where
    parseJSON (Object v) = do
        kind <- (v.: pack "kind")
        if (kind /= "Setup")
            then do mempty
            else do
                players <- (v.: pack "players")

                pTMap <- (v.: pack "plyerTroopsMap")
                pRem <- (v.: pack "playerRemaining")


                let pListR = map (\c -> (c, readMaybe $ (pTMap ! c) ! "owner")) countries
                let tListR = map (\c -> (c, readMaybe $ (pTMap ! c) ! "number_of_troops")) countries


                if (any (\p -> snd p == Nothing) pListR || any (\p -> snd p == Nothing) tListR || any (\p -> fst p == Nothing) cardsList)
                    then do mempty
                    else do
                        let playersMap = fromList $ map (\p -> (fst p, fromJust $ snd p)) pListR
                        let troopsMap = fromList $ map (\p -> (fst p, fromJust $ snd p)) tListR
                        let cardMap = fromList $ map (\p -> (fromJust $ fst p, snd p)) cardsList

                        return (InternalGameState troopsMap playersMap stdGen phase players getsCard discard deck cardMap)
        where
            countries = [(minBound ::Country)..]
    parseJSON _ = mempty
