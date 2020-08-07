---- Overloads ------------------------------
{-# LANGUAGE OverloadedStrings #-}


module Parse (
) where


---- Imports --------------------------------
import Message
import Data.Aeson
import Control.Applicative
import Data.Map (Map, fromList)

import qualified State as S
import RiskBoard

readRequest :: String -> Maybe Request
readRequest = undefined
--showResponse ::  Response -> String

phaseToJson:: S.Phase -> Value
phaseToJson (S.Attack (S.WonBattle ac dc na)) =
    object ["type" .= show "miniPhase",
            "attackingCountry" .= show ac,
            "defendingCountry" .= show dc,
            "attackersRemaining" .= fromEnum na]
phaseToJson p = object ["type" .= show "simplePhase",
                        "phase" .= show p]

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
                "players" .= map show (S.turnOrder g),
                "board" .= ((fromList.zip (map show countries)) $ map getOwnerTroopMap countries),
                "phase" .= (phaseToJson $ S.phase g)]
        where
            countries = [(minBound :: Country)..]
            getOwnerTroopMap:: Country -> Map String (Either Int String)
            getOwnerTroopMap c = fromList [("numberTroops", Left $ S.troops g c), ("owner", Right $ show $ S.owner g c)]



--    toJSON (Special (NumDefenders (S.WonBattle ac dc _)) p) =
--        object ["type" .= show "chooseDefenders",
--                "attackingCountry" .= ac,
--                "defendingCountry" .= dc]
--        where countries = [(minBound :: Country)..]

    toJSON (Invalid InvalidMove) = undefined
    toJSON (Invalid NotTurn) = undefined
