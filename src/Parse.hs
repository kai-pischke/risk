---- Overloads ------------------------------
--{-# LANGUAGE OverloadedStrings #-}


module Parse (
readRequest,
showResponse
) where


---- Imports --------------------------------
import Message
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Map (Map, fromList)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack)

import qualified State as S (MiniPhase(..), Phase(..), turnOrder, phase, troops, owner)
import RiskBoard
import GameElements
---------------------------------------------

readRequest :: ByteString -> Maybe Request
readRequest = decode
showResponse ::  Response -> ByteString
showResponse = encode




instance FromJSON Request where
    parseJSON (Object v) = do
        sender <- v .: pack "sender"
        requestType  <- v .: pack "action"
        if (requestType == ("StartGame" :: String))
            then return (Request (read sender) StartGame)
        else if (requestType == ("PlaceTroop" :: String))
            then do
                c <- v .: pack "country"
                return (Request (read sender) (PlaceTroop $ read c))
        else if (requestType == ("Attack" :: String))
            then do
                ac <- v .: pack "attacking_country"
                dc <- v .: pack "defending_country"
                na <- v .: pack "number_of_attackers"
                return (Request (read sender) (Attack (read ac) (read dc) (toEnum na)))
        else if (requestType == ("Reinforce" :: String))
            then do
                troops <- v.: pack "troops"
                return (Request (read sender) (Reinforce $ map (\x -> (read $ fst x, snd x)) troops))
        else if (requestType == ("Fortify" :: String))
            then do
                fc <- v .: pack "from_country"
                tc <- v .: pack "to_country"
                nt <- v .: pack "number_of_troops"
                return (Request (read sender) (Fortify (read fc) (read tc) nt))
        else if (requestType == ("Invade" :: String))
            then do
                nt <- v .: pack "number_of_troops"
                return (Request (read sender) (Invade nt))
        else if (requestType == ("ChooseDefenders" :: String))
            then do
                nd <- v .: pack "number_of_defenders"
                return (Request (read sender) (ChooseDefenders $ toEnum nd))
        else if (requestType == ("EndAttack" :: String))
            then return (Request (read sender) EndAttack)
        else if (requestType == ("SkipFortify" :: String))
            then return (Request (read sender) SkipFortify)
        else empty
    parseJSON _ = empty



---- ToJSON ---------------------------------

phaseToJson:: S.Phase -> Value
phaseToJson (S.Attack (S.WonBattle ac dc na)) =
    object [pack "kind" .= pack "BattleEnd",
            pack "attacking_country" .= (pack $ show ac),
            pack "defending_country" .= (pack $ show dc),
            pack "attackers_remaining" .= (fromEnum na)]
phaseToJson p = object [pack "kind" .= pack "Simple",
                        pack "phase" .= (pack $ show p)]

-- I use show and then a string here to avoid an error with haskell forcing it
--  to be a string despite having OverloadedStrings
instance ToJSON Response where
---- General Updates ------------------------

    toJSON (General (WaitingRoom ps)) =
        object [pack "kind" .= pack "State",
                pack "state" .= pack "WaitingRoom",
                pack "players" .= map show ps]

    toJSON (General (Setup setup)) =
        object [pack "kind" .= pack "State",
                pack "state" .= pack "Setup",
                pack "players" .= map show [1]] --Flag

    toJSON (General (Play g)) =
        object [pack "kind" .= pack "State",
                pack "state" .= pack "Play",
                pack "players" .= map (pack.show) (S.turnOrder g),
                pack "board" .= ((fromList.zip (map show countries)) $ map getOwnerTroopMap countries),
                pack "phase" .= (phaseToJson $ S.phase g)]
        where
            countries = [(minBound :: Country)..]
            getOwnerTroopMap:: Country -> Map String (Either Int String)
            getOwnerTroopMap c = fromList [("number_of_troops", Left $ S.troops g c), ("owner", Right $ show $ S.owner g c)]

---- Special Questions ----------------------

    toJSON (Special NumDefenders p) =
        object [pack "kind" .= pack "Question",
                pack "question" .= pack "ChooseDefenders"]

---- Invalid Errors -------------------------
    toJSON (Invalid e p) =
        object [pack "kind".= pack "Error",
                pack "error" .= (pack $ show e)]
