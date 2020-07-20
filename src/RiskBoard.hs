module RiskBoard 
    ( Country(..),
      neighbors,
      isNeighbor 
    ) where
    
import qualified Graph

data Country = Alaska
             | Alberta
             | CentralAmerica
             | EasternUnitedStates
             | Greenland
             | NorthwestTerritory
             | Ontario
             | Quebec
             | WesternUnitedStates
             | Argentina
             | Brazil
             | Peru
             | Venezuela
             | GreatBritain
             | Iceland
             | NorthernEurope
             | Scandinavia
             | SouthernEurope
             | Ukraine
             | WesternEurope
             | Congo
             | EastAfrica
             | Egypt
             | Madagascar
             | NorthAfrica
             | SouthAfrica 
             | Afghanistan
             | China
             | India
             | Irkutsk
             | Japan
             | Kamchatka
             | MiddleEast
             | Mongolia
             | Siam
             | Siberia
             | Ural
             | Yakutsk
             | EasternAustralia
             | Indonesia
             | NewGuinea
             | WesternAustralia
             deriving (Enum, Eq, Show, Ord)

neighbors :: Country -> [Country]
neighbors = undefined 

isNeighbor :: Country -> Country -> Bool
isNeighbor = undefined