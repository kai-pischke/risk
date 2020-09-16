module ParsePart (
Switch(..),
Owner(..)
) where

import Data.Aeson
import System.Random.Internal
import System.Random.SplitMix (seedSMGen', unseedSMGen)
import Data.Text (pack, unpack)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)

import GameElements
import RiskBoard

---- Helper DataType ------------------------
data Switch a b = LSwitch a | RSwitch b

instance (ToJSON a , ToJSON b) => ToJSON (Switch a b) where
    toJSON (RSwitch x) = toJSON x
    toJSON (LSwitch x) = toJSON x


data Owner = Owner Player | Unowned

instance ToJSON (Owner) where
    toJSON (Owner p) = String $ pack $ show p
    toJSON Unowned = String $ pack "Unowned"


instance ToJSON StdGen where
    toJSON (StdGen sm) =
        object [pack "seed" .= s,
                pack "gamma" .= g]
        where (s,g) = unseedSMGen sm

instance FromJSON StdGen where
    parseJSON (Object v) = do
        s <- (v.: pack "seed")
        g <- (v.: pack "gamma")
        return (StdGen (seedSMGen' (s, g)))
    parseJSON _ = mempty

instance ToJSON Player where
    toJSON (p) = toJSON $ show p

instance FromJSON Player where
    parseJSON (String s) = do
        let sR = readMaybe $ unpack s
        if (sR == Nothing)
            then do mempty
            else do return (fromJust sR)
    parseJSON _ = mempty

instance ToJSON Card where
    toJSON (c) = toJSON $ show c

instance FromJSON Card where
    parseJSON (String s) = do
        let sR = readMaybe $ unpack s
        if (sR == Nothing)
            then do mempty
            else do return (fromJust sR)
    parseJSON _ = mempty


instance ToJSON TradeIn where
    toJSON (None) = toJSON ([] :: [Value])
    toJSON (OneSet (c1, c2, c3)) = toJSON [[c1,c2,c3]]
    toJSON (TwoSet (c1, c2, c3) (c4, c5, c6)) = toJSON [[c1,c2,c3], [c4, c5, c6]]

instance FromJSON TradeIn where
    parseJSON (ar) = do
        a <- parseJSON ar
        if (length a == 0)
            then do return None
        else if (length a == 1)
            then do
                let cs = head a
                if (length cs == 3)
                    then do return (OneSet (head cs, (head.tail) cs, (head.tail.tail) cs))
                    else do mempty
        else if (length a == 2)
            then do
                let cs1 = head a
                let cs2 = head (tail a)
                if (length cs1 == 3 && length cs2 == 3)
                    then do return (TwoSet (head cs1, (head.tail) cs1, (head.tail.tail) cs1) (head cs2, (head.tail) cs2, (head.tail.tail) cs2))
                    else do mempty
        else do mempty

instance ToJSON Country where
    toJSON c = toJSON $ show c

instance FromJSON Country where
    parseJSON (String s) = do
        let sR = readMaybe $ unpack s
        if (sR == Nothing)
            then do mempty
            else do return (fromJust sR)
    parseJSON _ = mempty

instance FromJSONKey Country
