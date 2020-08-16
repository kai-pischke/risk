import Data.Aeson
import Data.Text (pack)

data Switch a b = RSwitch a | LSwitch b

instance (ToJSON a , ToJSON b) => ToJSON (Switch a b) where
    toJSON (RSwitch x) = toJSON x
    toJSON (LSwitch x) = toJSON x
