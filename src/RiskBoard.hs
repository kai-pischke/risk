module RiskBoard
    ( Country(..),
      Continent,
      neighbours,
      isNeighbour,
      northAmerica,
      europe,
      asia,
      southAmerica,
      africa,
      australia
    ) where

import qualified Graph

type Continent = [Country]

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
             deriving (Enum, Eq, Ord, Bounded)

instance Read Country where
    readsPrec _ "Alaska" = [(Alaska,"")]
    readsPrec _ "Alberta" = [(Alberta,"")]
    readsPrec _ "Central America" = [(CentralAmerica,"")]
    readsPrec _ "Eastern United States" = [(EasternUnitedStates,"")]
    readsPrec _ "Greenland" = [(Greenland,"")]
    readsPrec _ "Northwest Territory" = [(NorthwestTerritory,"")]
    readsPrec _ "Ontario" = [(Ontario,"")]
    readsPrec _ "Quebec" = [(Quebec,"")]
    readsPrec _ "Western United States" = [(WesternUnitedStates,"")]
    readsPrec _ "Argentina" = [(Argentina,"")]
    readsPrec _ "Brazil" = [(Brazil,"")]
    readsPrec _ "Peru" = [(Peru,"")]
    readsPrec _ "Venezuela" = [(Venezuela,"")]
    readsPrec _ "Great Britain" = [(GreatBritain,"")]
    readsPrec _ "Iceland" = [(Iceland,"")]
    readsPrec _ "Northern Europe" = [(NorthernEurope,"")]
    readsPrec _ "Scandinavia" = [(Scandinavia,"")]
    readsPrec _ "Southern Europe" = [(SouthernEurope,"")]
    readsPrec _ "Ukraine" = [(Ukraine,"")]
    readsPrec _ "Western Europe" = [(WesternEurope,"")]
    readsPrec _ "Congo" = [(Congo,"")]
    readsPrec _ "East Africa" = [(EastAfrica,"")]
    readsPrec _ "Egypt" = [(Egypt,"")]
    readsPrec _ "Madagascar" = [(Madagascar,"")]
    readsPrec _ "North Africa" = [(NorthAfrica,"")]
    readsPrec _ "South Africa" = [(SouthAfrica,"")]
    readsPrec _ "Afghanistan" = [(Afghanistan,"")]
    readsPrec _ "China" = [(China,"")]
    readsPrec _ "India" = [(India,"")]
    readsPrec _ "Irkutsk" = [(Irkutsk,"")]
    readsPrec _ "Japan" = [(Japan,"")]
    readsPrec _ "Kamchatka" = [(Kamchatka,"")]
    readsPrec _ "Middle East" = [(MiddleEast,"")]
    readsPrec _ "Mongolia" = [(Mongolia,"")]
    readsPrec _ "Siam" = [(Siam,"")]
    readsPrec _ "Siberia" = [(Siberia,"")]
    readsPrec _ "Ural" = [(Ural,"")]
    readsPrec _ "Yakutsk" = [(Yakutsk,"")]
    readsPrec _ "Eastern Australia" = [(EasternAustralia,"")]
    readsPrec _ "Indonesia" = [(Indonesia,"")]
    readsPrec _ "New Guinea" = [(NewGuinea,"")]
    readsPrec _ "Western Australia" = [(WesternAustralia,"")]
    readsPrec _ _ = []


instance Show Country where
    show Alaska = "Alaska"
    show Alberta = "Alberta"
    show CentralAmerica = "Central America"
    show EasternUnitedStates = "Eastern United States"
    show Greenland = "Greenland"
    show NorthwestTerritory = "Northwest Territory"
    show Ontario = "Ontario"
    show Quebec = "Quebec"
    show WesternUnitedStates = "Western United States"
    show Argentina = "Argentina"
    show Brazil = "Brazil"
    show Peru = "Peru"
    show Venezuela = "Venezuela"
    show GreatBritain = "Great Britain"
    show Iceland = "Iceland"
    show NorthernEurope = "Northern Europe"
    show Scandinavia = "Scandinavia"
    show SouthernEurope = "Southern Europe"
    show Ukraine = "Ukraine"
    show WesternEurope = "Western Europe"
    show Congo = "Congo"
    show EastAfrica = "East Africa"
    show Egypt = "Egypt"
    show Madagascar = "Madagascar"
    show NorthAfrica = "North Africa"
    show SouthAfrica = "South Africa"
    show Afghanistan = "Afghanistan"
    show China = "China"
    show India = "India"
    show Irkutsk = "Irkutsk"
    show Japan = "Japan"
    show Kamchatka = "Kamchatka"
    show MiddleEast = "Middle East"
    show Mongolia = "Mongolia"
    show Siam = "Siam"
    show Siberia = "Siberia"
    show Ural = "Ural"
    show Yakutsk = "Yakutsk"
    show EasternAustralia = "Eastern Australia"
    show Indonesia = "Indonesia"
    show NewGuinea = "New Guinea"
    show WesternAustralia = "Western Australia"

--Riskboard made from adjacency list
riskBoard = Graph.makeGraph [(Alaska , [Alberta, NorthwestTerritory,Kamchatka]),
                            (Alberta , [Alaska, NorthwestTerritory, Ontario, WesternUnitedStates]),
                            (CentralAmerica , [EasternUnitedStates, WesternUnitedStates, Venezuela]),
                            (EasternUnitedStates , [CentralAmerica, Ontario, Quebec, WesternUnitedStates]),
                            (Greenland , [Ontario, NorthwestTerritory, Quebec, Iceland]),
                            (NorthwestTerritory , [Alaska, Alberta, Ontario, Greenland]),
                            (Ontario , [NorthwestTerritory, Alberta, WesternUnitedStates, EasternUnitedStates, Quebec, Greenland]),
                            (Quebec , [Ontario, EasternUnitedStates, Greenland]),
                            (WesternUnitedStates , [Alberta, Ontario, EasternUnitedStates, CentralAmerica]),
                            (Argentina , [Peru, Brazil]),
                            (Brazil , [Venezuela, Peru, Argentina, NorthAfrica]),
                            (Peru , [Venezuela, Brazil, Argentina]),
                            (Venezuela , [CentralAmerica, Brazil, Peru]),
                            (GreatBritain , [Iceland, Scandinavia, NorthernEurope, WesternEurope]),
                            (Iceland , [Greenland, Scandinavia, GreatBritain]),
                            (NorthernEurope , [GreatBritain, Scandinavia, Ukraine, SouthernEurope, WesternEurope]),
                            (Scandinavia , [Iceland, Ukraine, NorthernEurope, GreatBritain]),
                            (SouthernEurope , [WesternEurope, NorthernEurope, Ukraine, MiddleEast, Egypt, NorthAfrica]),
                            (Ukraine , [Scandinavia, Ural, Afghanistan, MiddleEast, SouthernEurope, NorthernEurope]),
                            (WesternEurope , [GreatBritain, NorthernEurope, SouthernEurope, NorthAfrica]),
                            (Congo , [NorthAfrica, EastAfrica, SouthAfrica]),
                            (EastAfrica , [Egypt, NorthAfrica, Congo, SouthAfrica, MiddleEast, Madagascar]),
                            (Egypt , [SouthernEurope, MiddleEast, EastAfrica, NorthAfrica]),
                            (Madagascar , [EastAfrica, SouthAfrica]),
                            (NorthAfrica , [Brazil, WesternEurope, SouthernEurope, Egypt, EastAfrica, Congo]),
                            (SouthAfrica , [Congo, EastAfrica, Madagascar]),
                            (Afghanistan , [Ukraine, Ural, China, India, MiddleEast]),
                            (China , [Ural, Siberia, Mongolia, Siam, India, Afghanistan]),
                            (India , [MiddleEast, Afghanistan, China, Siam]),
                            (MiddleEast , [Ukraine, Afghanistan, India, EastAfrica, Egypt, SouthernEurope]),
                            (Irkutsk , [Siberia, Yakutsk, Kamchatka, Mongolia]),
                            (Japan , [Mongolia, Kamchatka]),
                            (Kamchatka , [Mongolia, Irkutsk, Yakutsk, Alaska, Japan]),
                            (Mongolia , [Siberia, Irkutsk, Kamchatka, Japan, China]),
                            (Siam , [India, China, Indonesia]),
                            (Siberia , [Ural, Yakutsk, Irkutsk, Mongolia, China]),
                            (Ural , [Ukraine, Siberia, China, Afghanistan]),
                            (Yakutsk , [Siberia, Kamchatka, Irkutsk]),
                            (EasternAustralia , [NewGuinea, WesternAustralia]),
                            (Indonesia , [Siam, NewGuinea, WesternAustralia]),
                            (NewGuinea ,  [Indonesia, WesternAustralia, EasternAustralia]),
                            (WesternAustralia , [Indonesia, NewGuinea, EasternAustralia])]

northAmerica :: Continent
northAmerica = [toEnum 0 .. toEnum 8]

southAmerica :: Continent
southAmerica = [toEnum 9 .. toEnum 12]

europe :: Continent
europe = [toEnum 13 .. toEnum 19]

africa :: Continent
africa = [toEnum 20 .. toEnum 25]

asia :: Continent
asia = [toEnum 26 .. toEnum 37]

australia :: Continent
australia = [toEnum 38 .. toEnum 41]







-- Simple Renamings
neighbours :: Country -> [Country]
neighbours = Graph.neighbours riskBoard

isNeighbour :: Country -> Country -> Bool
isNeighbour = Graph.isNeighbour riskBoard
