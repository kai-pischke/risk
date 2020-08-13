---- Overloads ------------------------------
{-# LANGUAGE OverloadedStrings #-}


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

import qualified State as S (MiniPhase(..), Phase(..), turnOrder, phase, troops, owner)
import RiskBoard
import GameElements
---------------------------------------------

decodeRequest :: ByteString -> Either Request ParseError
decodeRequest = maybe (Right parseError) Left . decode 

encodeResponse :: Response -> ByteString
encodeResponse = encode

type ParseError = ByteString

parseError :: ParseError
parseError = "{\"Invalid JSON\"}"

instance FromJSON Request where
    parseJSON (Object v) = do
        sender <- v .: "sender"
        requestType  <- v .: "action"
        if (requestType == ("StartGame" :: String))
            then return (Request (read sender) StartGame)
        else if (requestType == ("PlaceTroop" :: String))
            then do
                c <- v .: "country"
                return (Request (read sender) (PlaceTroop $ read c))
        else if (requestType == ("Attack" :: String))
            then do
                ac <- v .: "attacking_country"
                dc <- v .: "defending_country"
                na <- v .: "number_of_attackers"
                return (Request (read sender) (Attack (read ac) (read dc) (toEnum na)))
        else if (requestType == ("Reinforce" :: String))
            then do
                troops <- v.: "troops"
                return (Request (read sender) (Reinforce $ map (\x -> (read $ fst x, snd x)) troops))
        else if (requestType == ("Fortify" :: String))
            then do
                fc <- v .: "from_country"
                tc <- v .: "to_country"
                nt <- v .: "number_of_troops"
                return (Request (read sender) (Fortify (read fc) (read tc) nt))
        else if (requestType == ("Invade" :: String))
            then do
                nt <- v .: "number_of_troops"
                return (Request (read sender) (Invade nt))
        else if (requestType == ("ChooseDefenders" :: String))
            then do
                nd <- v .: "number_of_defenders"
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
    object ["kind" .= show "BattleEnd",
            "attacking_country" .= show ac,
            "defending_country" .= show dc,
            "attackers_remaining" .= fromEnum na]
phaseToJson p = object ["kind" .= show "Simple",
                        "phase" .= show p]

-- I use show and then a string here to avoid an error with haskell forcing it
--  to be a string despite having OverloadedStrings
instance ToJSON Response where
---- General Updates ------------------------

    toJSON (General (WaitingRoom ps)) =
        object ["kind" .= show "State",
                "state" .= show "WaitingRoom",
                "players" .= map show ps]

    toJSON (General (Setup setup)) =
        object ["kind" .= show "State",
                "state" .= show "Setup",
                "players" .= map show [1]] --Flag

    toJSON (General (Play g)) =
        object ["kind" .= show "State",
                "state" .= show "Play",
                "players" .= map show (S.turnOrder g),
                "board" .= ((fromList.zip (map show countries)) $ map getOwnerTroopMap countries),
                "phase" .= (phaseToJson $ S.phase g)]
        where
            countries = [(minBound :: Country)..]
            getOwnerTroopMap:: Country -> Map String (Either Int String)
            getOwnerTroopMap c = fromList [("number_of_troops", Left $ S.troops g c), ("owner", Right $ show $ S.owner g c)]

---- Special Questions ----------------------

    toJSON (Special NumDefenders p) =
        object ["kind" .= show "Question",
                "questoiun" .= show "ChooseDefenders"]

---- Invalid Errors -------------------------
    toJSON (Invalid e p) =
        object ["kind".= show "Error",
                "error" .= show e]
