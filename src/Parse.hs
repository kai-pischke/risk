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
import Data.Map(assocs)

import SetupBoard
import qualified State as S (turnOrder, phase, troops, owner, cards)
import RiskBoard (Country)
import GameElements (Player)
import ParsePart
---------------------------------------------

data Switch a b = LSwitch a | RSwitch b

instance (ToJSON a , ToJSON b) => ToJSON (Switch a b) where
    toJSON (RSwitch x) = toJSON x
    toJSON (LSwitch x) = toJSON x


setupBoardOwner:: SetupState -> Country -> (Maybe Player, Int)
setupBoardOwner (Incomplete s) c = incompleteBoardOwner (Incomplete s) c
setupBoardOwner (PartiallyComplete s)  c = (Just p, n)
    where
        (p,n) = (partiallyCompleteBoardOwner (PartiallyComplete s) c)

setupBoardOwner s c = (Just p, n)
    where
        (p,n) = (completeBoardOwner s c)


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
parseError = encode $ object [pack "kind".= pack "Error",
                     pack "error" .= "ParseError"]

---- Instances For To/From JSON -------------

instance FromJSON Request where
    parseJSON (Object v) = do
        sender <- (v .: pack "sender")
        requestType  <- (v .: pack "action")

        if (requestType == ("StartGame" :: String))
            then return (Request sender StartGame)
        else if (requestType == ("SaveGame" :: String))
            then return (Request sender SaveGame)
        else if (requestType == "PlaceTroop")
            then do
                c <- (v .: pack "country")
                return (Request sender (PlaceTroop c))
        else if (requestType == "Attack")
            then do
                ac <- v .: pack "attacking_country"
                dc <- v .: pack "defending_country"
                na <- v .: pack "number_of_attackers"
                if (na < 1 || na >3)
                    then do mempty
                    else do return (Request sender (Attack ac dc (toEnum na)))
        else if (requestType == ("Reinforce" :: String))
            then do
                t <- (v.: pack "troops")
                tradeIn  <- (v.: pack "trade_in")


                let troopsR = map (\p -> (readMaybe $ fst p, snd p)) $ assocs t

                if (any (\p -> fst p == Nothing) troopsR)
                    then do mempty
                    else do return (Request sender (Reinforce tradeIn (map (\p -> (fromJust $ fst p, snd p)) troopsR)))
        else if (requestType == ("Trade" :: String))
            then do
                t <- v.: pack "troops"
                tradeIn <- v.: pack "trade_in"

                let troopsR = map (\p -> (readMaybe $ fst p, snd p)) $ assocs t

                if (any (\p -> fst p == Nothing) troopsR)-- (any (\p -> (typeOf $ snd p) /= typeOf (1 :: Int)) troopsR)
                    then do mempty
                    else do return (Request sender (Trade tradeIn (map (\p -> (fromJust $ fst p, snd p)) troopsR)))
        else if (requestType == ("Fortify" :: String))
            then do
                fc <- v .: pack "from_country"
                tc <- v .: pack "to_country"
                nt <- v .: pack "number_of_troops"
                return (Request sender (Fortify fc tc nt))
        else if (requestType == ("Invade" :: String))
            then do
                nt <- v .: pack "number_of_troops"
                return (Request sender (Invade nt))
        else if (requestType == ("ChooseDefenders" :: String))
            then do
                nd <- v .: pack "number_of_defenders"
                if (nd <1 || nd >2)
                    then do mempty
                    else do return (Request sender (ChooseDefenders $ toEnum nd))
        else if (requestType == ("EndAttack" :: String))
            then return (Request sender EndAttack)
        else if (requestType == ("SkipFortify" :: String))
            then return (Request sender SkipFortify)

        else do mempty
    parseJSON _ = mempty



---- ToJSON ---------------------------------

instance ToJSON Response where
---- General Updates ------------------------

    toJSON (General (WaitingRoom ps)) =
        object [pack "kind" .= pack "State",
                pack "state" .= pack "WaitingRoom",
                pack "players" .= ps]

    toJSON (General (Setup setup)) =
        object [pack "kind" .= pack "State",
                pack "state" .= pack "Setup",
                pack "players" .= (setUpTurnOrder setup),
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
                pack "players" .= (ps),
                pack "board" .= ((fromList.zip (map show countries)) $ map getOwnerTroopMap countries),
                pack "phase" .= (S.phase g),
                pack "cards" .= cardmap]
        where
            countries = [(minBound :: Country)..]
            ps = S.turnOrder g
            cardmap = fromList $ zip (map show ps) (map (S.cards g) ps)
            getOwnerTroopMap:: Country -> Map String (Switch Int Player)
            getOwnerTroopMap c = fromList [("number_of_troops", LSwitch $ S.troops g c), ("owner", RSwitch $ S.owner g c)]

---- Special Questions ----------------------

    toJSON (Special q p) =
        object [pack "kind" .= pack "Question",
                pack "player" .= p,
                pack "question" .= show q]

---- Invalid Errors -------------------------
    toJSON (Invalid e p) =
        object [pack "kind".= pack "Error",
                pack "error" .= show e,
                pack "player" .= p]

---- Game Won -------------------------------
    toJSON (GameWon p) =
        object [pack "kind" .= pack "Won",
                pack "winner" .= p]
