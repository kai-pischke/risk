module RiskBoard 
    ( Country(..),
      neighbours,
      isNeighbour 
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
             deriving (Enum, Eq, Show, Ord, Bounded)


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

-- Simple Renamings
neighbours :: Country -> [Country]
neighbours = Graph.neighbours riskBoard

isNeighbour :: Country -> Country -> Bool
isNeighbour = Graph.isNeighbour riskBoard
