---- Overloads ------------------------------
--{-# LANGUAGE OverloadedStrings #-}


module Parse (
decodeRequest,
encodeResponse,
ParseError
) where


---- Imports --------------------------------
import Message
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Map (Map, fromList)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Text (pack)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Data.Typeable (typeOf)

import SetupBoard
import qualified State as S (MiniPhase(..), Phase(..), turnOrder, phase, troops, owner)
import RiskBoard
import GameElements
---------------------------------------------

---- Helpter DataType -----------------------
data Switch a b = LSwitch a | RSwitch b

instance (ToJSON a , ToJSON b) => ToJSON (Switch a b) where
    toJSON (RSwitch x) = toJSON x
    toJSON (LSwitch x) = toJSON x
---------------------------------------------



---- Public Functions -----------------------

decodeRequest :: ByteString -> Either Request ParseError
decodeRequest = maybe (Right parseError) Left . decode

encodeResponse :: Response -> ByteString
encodeResponse = encode

--setupBoardOwner:: SetupState -> Country -> (Player, Int)
--setupBoardOwner (Incomplete s) = incompleteBoardOwner (Incomplete s)
--setupBoardOwner s = completeBoardOwner s

type ParseError = ByteString

parseError :: ParseError
parseError = fromString "{\"Invalid JSON\"}"


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
                    let troopsR = map (\x -> (readMaybe $ fst x, snd x)) troops
                    if (any (\p -> fst p == Nothing) troopsR)
                        then do mempty
                        else do return (Request (fromJust senderR) (Reinforce $ map (\p -> (fromJust $ fst p, snd p)) troopsR))
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

phaseToJson:: S.Phase -> Value
phaseToJson (S.Attack (S.WonBattle ac dc na)) =
    object [pack "kind" .= pack "BattleEnd",
            pack "attacking_country" .= (pack $ show ac),
            pack "defending_country" .= (pack $ show dc),
            pack "attackers_remaining" .= (fromEnum na)]
phaseToJson p = object [pack "kind" .= pack "Simple",
                        pack "phase" .= (pack $ show p)]


instance ToJSON Response where
---- General Updates ------------------------

    toJSON (General (WaitingRoom ps)) =
        object [pack "kind" .= pack "State",
                pack "state" .= pack "WaitingRoom",
                pack "players" .= map show ps]

    toJSON (General (Setup setup)) =
        object [pack "kind" .= pack "State",
                pack "state" .= pack "Setup",
                pack "players" .= map (pack.show) ["hi I'm an error"]]--(setupTurnOrder setup)]
--                pack "board" .= ]

    toJSON (General (Play g)) =
        object [pack "kind" .= pack "State",
                pack "state" .= pack "Play",
                pack "players" .= map (pack.show) (S.turnOrder g),
                pack "board" .= ((fromList.zip (map show countries)) $ map getOwnerTroopMap countries),
                pack "phase" .= (phaseToJson $ S.phase g)]
        where
            countries = [(minBound :: Country)..]
            getOwnerTroopMap:: Country -> Map String (Switch Int String)
            getOwnerTroopMap c = fromList [("number_of_troops", LSwitch $ S.troops g c), ("owner", RSwitch $ show $ S.owner g c)]

---- Special Questions ----------------------

    toJSON (Special NumDefenders p) =
        object [pack "kind" .= pack "Question",
                pack "player" .= (pack $ show p),
                pack "question" .= pack "ChooseDefenders"]

---- Invalid Errors -------------------------
    toJSON (Invalid e p) =
        object [pack "kind".= pack "Error",
                pack "error" .= (pack $ show e),
                pack "player" .= (pack $ show p)]
