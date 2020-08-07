---- Overloads ------------------------------
{-# LANGUAGE OverloadedStrings #-}


module Parse (
) where


---- Imports --------------------------------
import Message
import Data.Aeson
import Control.Applicative
import Data.Map (Map)

import State
import RiskBoard

readRequest :: String -> Maybe Request
readRequest = undefined
--showResponse ::  Response -> String

phaseToJson:: Phase -> Value
phaseToJson (Attack (WonBattle ac dc na)) =
    object ["type" .= show "miniPhase",
            "attackingCountry" .= ac,
            "defendingCountry" .= dc,
            "attackersRemaining" .= na]
phaseToJson p = object ["type" .= show "simplePhase",
                        "phase": show p]

-- I use show and then a string here to avoid an error with haskell
--  forcing it to be a string despite having OverloadedStrings
instance ToJSON Response where
    toJSON (General (WaitingRoom ps)) =
        object ["type" .= show "waitingRoom",
                "players" .= map show ps]

    toJSON (General (Setup setup)) =
        object ["type" .= show "setup",
                "players" .= map show [1]]

    toJSON (General (Play g)) =
        object ["type" .= show "play",
                "players" .= map show (turnOrder g),
                "board" .= (encode.fromList.zip (map show countries)) $ map (\c -> (show $ owner g c, troops g c)) countries,
                "phase" .= phaseToJson $ phase g
        where countries = [(minBound :: Country)..]



    toJSON (Special NumDefenders p) =
        object ["type" .= show "chooseDefenders",
                "attackingCountry" .= ,
                "defendingCountry" .=
        where countries = [(minBound :: Country)..]

    toJSON (Invalid InvalidMove) = undefined
    toJSON (Invalid NotTurn) = undefined
