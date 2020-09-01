{-|
Module      : Parse
Description : JSON encoding and decoding.
Maintainer  : River

This module handles the encoding and decoding of JSON responses and requests.
-}


module Parse (
decodeRequest,
encodeResponse,
ParseError
) where


---- Imports --------------------------------
import Message
import Data.Aeson
import Data.Map (Map, fromList)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Text (pack)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Data.Typeable (typeOf)
import Data.Map(assocs)

import SetupBoard
import qualified State as S (MiniPhase(..), Phase(..), turnOrder, phase, troops, owner, cards)
import RiskBoard (Country)
import GameElements (Player, TradeIn(..), Card)
---------------------------------------------


---- Helper DataType ------------------------
data Switch a b = LSwitch a | RSwitch b

instance (ToJSON a , ToJSON b) => ToJSON (Switch a b) where
    toJSON (RSwitch x) = toJSON x
    toJSON (LSwitch x) = toJSON x

data Owner = Owner Player | Unowned

instance ToJSON (Owner) where
    toJSON (Owner p) = String $ pack $ show p
    toJSON Unowned = String $ pack "Unowned"

---- Helper Functions -----------------------

phaseToJson:: S.Phase -> Value
phaseToJson (S.Attack (S.WonBattle ac dc na)) =
    object [pack "kind" .= pack "BattleEnd",
            pack "attacking_country" .= (pack $ show ac),
            pack "defending_country" .= (pack $ show dc),
            pack "attackers_remaining" .= (fromEnum na)]

phaseToJson (S.Attack (S.MidBattle ac dc na)) =
    object [pack "kind" .= pack "MidBattle",
            pack "attacking_country" .= (pack $ show ac),
            pack "defending_country" .= (pack $ show dc),
            pack "attackers" .= (fromEnum na)]

phaseToJson (S.Attack (S.Normal)) =
    object [pack "kind" .= pack "Simple",
            pack "phase" .= (pack "Attack")]

phaseToJson p = object [pack "kind" .= pack "Simple",
                        pack "phase" .= (pack $ show p)]

setupBoardOwner:: SetupState -> Country -> (Maybe Player, Int)
setupBoardOwner (Incomplete s) c = incompleteBoardOwner (Incomplete s) c
setupBoardOwner (PartiallyComplete s)  c = (Just p, n)
    where
        (p,n) = (partiallyCompleteBoardOwner (PartiallyComplete s) c)

setupBoardOwner s c = (Just p, n)
    where
        (p,n) = (completeBoardOwner s c)

        
tradeInFromJson:: [String] -> Maybe (Card,Card,Card)
tradeInFromJson (c1:c2:c3:[])
    | (any (==Nothing) [c1R,c2R,c3R]) = Nothing
    | otherwise = Just (fromJust c1R, fromJust c2R, fromJust c3R)
    where 
        c1R = readMaybe c1
        c2R = readMaybe c2
        c3R = readMaybe c3

tradeInFromJson _ = Nothing

cardsListToTradeIn:: [Maybe (Card,Card,Card)] -> Maybe TradeIn
cardsListToTradeIn [] = Just None
cardsListToTradeIn (s1:[]) 
    | s1 == Nothing = Nothing
    | otherwise = Just $ OneSet $ fromJust s1
cardsListToTradeIn (s1:s2:[]) 
    | s1 == Nothing || s2 == Nothing = Nothing
    | otherwise = Just $ TwoSet (fromJust s1) (fromJust s2)
cardsListToTradeIn _ = Nothing
    
 


---- Public Functions -----------------------
-- | Reads in a received 'ByteString' and returns a 'Left' 'Request' or a 'Right' 'ParseError'.
decodeRequest :: ByteString -> Either Request ParseError
decodeRequest = maybe (Right parseError) Left . decode

-- | Encodes a 'Response' to a 'ByteString'.
encodeResponse :: Response -> ByteString
encodeResponse = encode


type ParseError = ByteString

-- | This is the 'ByteString' holding the JSON-encoded response to a parse error.
parseError :: ParseError
parseError = fromString "{\"Invalid JSON\"}"

---- Instances For To/From JSON -------------

instance FromJSON Request where
    parseJSON (Object v) = do
        sender <- (v .: pack "sender")
        let senderR = readMaybe sender
        if (senderR == Nothing)
            then do mempty
        else do
            requestType  <- v .: pack "action"
            if (requestType == ("StartGame" :: String))
                then return (Request (read sender) StartGame)
            else if (requestType == ("PlaceTroop" :: String))
                then do
                    c <- (v .: pack "country")
                    let cR = readMaybe c
                    if (cR == Nothing)
                        then do mempty
                        else do return (Request (fromJust senderR) (PlaceTroop $ fromJust cR))
            else if (requestType == ("Attack" :: String))
                then do
                    ac <- v .: pack "attacking_country"
                    let acR = readMaybe ac
                    dc <- v .: pack "defending_country"
                    let dcR = readMaybe dc
                    na <- v .: pack "number_of_attackers"
                    if (acR == Nothing || dcR == Nothing || not (typeOf na == typeOf (1 :: Int)) || na <1 || na>3)
                        then do mempty
                        else do return (Request (fromJust senderR) (Attack (fromJust acR) (fromJust dcR) (toEnum na)))
            else if (requestType == ("Reinforce" :: String))
                then do
                    troops <- v.: pack "troops"
                    t <- parseJSON troops
                    
                    cards <- v.: pack "tradein"
                    let tradeInR = cardsListToTradeIn $ map tradeInFromJson cards 
                    let troopsR = map (\p -> (readMaybe $ fst p, snd p)) $ assocs t
                    
                    if (tradeInR == Nothing || any (\p -> fst p == Nothing) troopsR)-- (any (\p -> (typeOf $ snd p) /= typeOf (1 :: Int)) troopsR)
                        then do mempty
                        else do return (Request (fromJust senderR) (Reinforce (fromJust tradeInR) (map (\p -> (fromJust $ fst p, snd p)) troopsR)))
            else if (requestType == ("Trade" :: String))
                then do
                    troops <- v.: pack "troops"
                    t <- parseJSON troops
                    
                    cards <- v.: pack "tradein"
                    let tradeInR = cardsListToTradeIn $ map tradeInFromJson cards 
                    let troopsR = map (\p -> (readMaybe $ fst p, snd p)) $ assocs t
                    
                    if (tradeInR == Nothing || any (\p -> fst p == Nothing) troopsR)-- (any (\p -> (typeOf $ snd p) /= typeOf (1 :: Int)) troopsR)
                        then do mempty
                        else do return (Request (fromJust senderR) (Trade (fromJust tradeInR) (map (\p -> (fromJust $ fst p, snd p)) troopsR)))
            else if (requestType == ("Fortify" :: String))
                then do
                    fc <- v .: pack "from_country"
                    let fcR = readMaybe fc
                    tc <- v .: pack "to_country"
                    let tcR = readMaybe tc
                    nt <- v .: pack "number_of_troops"
                    if (fcR == Nothing || tcR == Nothing || not (typeOf nt == typeOf (1 :: Int)))
                        then do mempty
                        else do return (Request (fromJust senderR) (Fortify (fromJust fcR) (fromJust tcR) nt))
            else if (requestType == ("Invade" :: String))
                then do
                    nt <- v .: pack "number_of_troops"
                    if (not (typeOf nt == typeOf (1 :: Int)))
                        then do mempty
                        else do return (Request (fromJust senderR) (Invade nt))
            else if (requestType == ("ChooseDefenders" :: String))
                then do
                    nd <- v .: pack "number_of_defenders"
                    if (not (typeOf nd == typeOf (1 :: Int)) || nd < 1 || nd > 2)
                        then do mempty
                        else do return (Request (fromJust senderR) (ChooseDefenders $ toEnum nd))
            else if (requestType == ("EndAttack" :: String))
                then return (Request (fromJust senderR) EndAttack)
            else if (requestType == ("SkipFortify" :: String))
                then return (Request (fromJust senderR) SkipFortify)
        
            else do mempty
    parseJSON _ = mempty



---- ToJSON ---------------------------------

instance ToJSON Response where
---- General Updates ------------------------

    toJSON (General (WaitingRoom ps)) =
        object [pack "kind" .= pack "State",
                pack "state" .= pack "WaitingRoom",
                pack "players" .= map show ps]

    toJSON (General (Setup setup)) =
        object [pack "kind" .= pack "State",
                pack "state" .= pack "Setup",
                pack "players" .= map (pack.show) (setUpTurnOrder setup),
                pack "board" .= ((fromList.zip (map show countries)) $ map getOwnerTroopMap countries)]
        where
            countries = [(minBound :: Country)..]
            getOwnerTroopMap:: Country -> Map String (Switch Int Owner)
            getOwnerTroopMap c = fromList [("number_of_troops", LSwitch n), ("owner", RSwitch owner)]
                where
                    (mp, n) = setupBoardOwner setup c
                    owner = if (mp ==Nothing) then Unowned else Owner $ fromJust mp


    toJSON (General (Play g)) =
        object [pack "kind" .= pack "State",
                pack "state" .= pack "Play",
                pack "players" .= map (pack.show) (ps),
                pack "board" .= ((fromList.zip (map show countries)) $ map getOwnerTroopMap countries),
                pack "phase" .= (phaseToJson $ S.phase g),
                pack "cards" .= cardmap]
        where
            countries = [(minBound :: Country)..]
            ps = S.turnOrder g
            cardmap = fromList $ zip (show ps) (map (map (show) .S.cards g) ps)
            getOwnerTroopMap:: Country -> Map String (Switch Int String)
            getOwnerTroopMap c = fromList [("number_of_troops", LSwitch $ S.troops g c), ("owner", RSwitch $ show $ S.owner g c)]

---- Special Questions ----------------------

    toJSON (Special q p) =
        object [pack "kind" .= pack "Question",
                pack "player" .= (pack $ show p),
                pack "question" .= (pack $ show q)]

---- Invalid Errors -------------------------
    toJSON (Invalid e p) =
        object [pack "kind".= pack "Error",
                pack "error" .= (pack $ show e),
                pack "player" .= (pack $ show p)]

---- Game Won -------------------------------
    toJSON (GameWon p) =
        object [pack "kind" .= pack "Won",
                pack "winner" .= (pack $ show p)]
