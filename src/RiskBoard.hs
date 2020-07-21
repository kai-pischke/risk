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
             deriving (Enum, Eq, Show, Ord)


neighbours :: Country -> [Country]
neighbours Alaska = [Alberta, NorthwestTerritory,Kamchatka]
neighbours Alberta = [Alaska, NorthwestTerritory, Ontario, WesternUnitedStates]
neighbours CentralAmerica = [EasternUnitedStates, WesternUnitedStates, Venezuela]
neighbours EasternUnitedStates = [CentralAmerica, Ontario, Quebec, WesternUnitedStates]
neighbours Greenland = [Ontario, NorthwestTerritory, Quebec, Iceland]
neighbours NorthwestTerritory = [Alaska, Alberta, Ontario, Greenland]
neighbours Ontario = [NorthwestTerritory, Alberta, WesternUnitedStates, EasternUnitedStates, Quebec, Greenland]
neighbours Quebec = [Ontario, EasternUnitedStates, Greenland]
neighbours WesternUnitedStates = [Alberta, Ontario, EasternUnitedStates, CentralAmerica]
neighbours Argentina = [Peru, Brazil]
neighbours Brazil = [Venezuela, Peru, Argentina, NorthAfrica]
neighbours Peru = [Venezuela, Brazil, Argentina]
neighbours Venezuela = [CentralAmerica, Brazil, Peru]
neighbours GreatBritain = [Iceland, Scandinavia, NorthernEurope, WesternEurope]
neighbours Iceland = [Greenland, Scandinavia, GreatBritain]
neighbours NorthernEurope = [GreatBritain, Scandinavia, Ukraine, SouthernEurope, WesternEurope]
neighbours Scandinavia = [Iceland, Ukraine, NorthernEurope, GreatBritain]
neighbours SouthernEurope = [WesternEurope, NorthernEurope, Ukraine, MiddleEast, Egypt, NorthAfrica]
neighbours Ukraine = [Scandinavia, Ural, Afghanistan, MiddleEast, SouthernEurope, NorthernEurope]
neighbours WesternEurope = [GreatBritain, NorthernEurope, SouthernEurope, NorthAfrica]
neighbours Congo = [NorthAfrica, EastAfrica, SouthAfrica]
neighbours EastAfrica = [Egypt, NorthAfrica, Congo, SouthAfrica, MiddleEast, Madagascar]
neighbours Egypt = [SouthernEurope, MiddleEast, EastAfrica, NorthAfrica]
neighbours Madagascar = [EastAfrica, SouthAfrica]
neighbours NorthAfrica = [Brazil, WesternEurope, SouthernEurope, Egypt, EastAfrica, Congo]
neighbours SouthAfrica = [Congo, EastAfrica, Madagascar]
neighbours Afghanistan = [Ukraine, Ural, China, India, MiddleEast]
neighbours China = [Ural, Siberia, Mongolia, Siam, India, Afghanistan]
neighbours India = [MiddleEast, Afghanistan, China, Siam]
neighbours MiddleEast = [Ukraine, Afghanistan, India, EastAfrica, Egypt, SouthernEurope]
neighbours Irkutsk = [Siberia, Yakutsk, Kamchatka, Mongolia]
neighbours Japan = [Mongolia, Kamchatka]
neighbours Kamchatka = [Mongolia, Irkutsk, Yakutsk, Alaska, Japan]
neighbours Mongolia = [Siberia, Irkutsk, Kamchatka, Japan, China]
neighbours Siam = [India, China, Indonesia]
neighbours Siberia = [Ural, Yakutsk, Irkutsk, Mongolia, China]
neighbours Ural = [Ukraine, Siberia, China, Afghanistan]
neighbours Yakutsk = [Siberia, Kamchatka, Irkutsk]
neighbours EasternAustralia = [NewGuinea, WesternAustralia]
neighbours Indonesia = [Siam, NewGuinea, WesternAustralia]
neighbours NewGuinea =  [Indonesia, WesternAustralia, EasternAustralia]
neighbours WesternAustralia = [Indonesia, NewGuinea, EasternAustralia]

isNeighbour :: Country -> Country -> Bool
isNeighbour c d = any (==d) (neighbours c)
