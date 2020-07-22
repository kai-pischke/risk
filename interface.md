
# Modules
## Graph
### Types
```hs
Eq n => Graph n
```

### Functions
```hs
neighbours :: Eq n => Graph n -> n -> [n]
```  
Given a graph and a node, returns a list of the directly adjacent nodes.
```hs
isNeighbour :: Eq n => Graph n -> n -> n -> Bool
```  
Given a graph and two nodes, returns `True` if there is an edge connecting them and `False` otherwise.
```hs
makeGraph :: Eq n => [(n, [n])] -> Graph n
```
Given a valid adjacency list for an undirected graph, returns the corresponding representation as a `Graph n`.

## RiskMap

### Types
```hs
Country -- (Enum, Eq, Show, Ord)
```
The `Country` type should be inhabited by 42 nullary constructors with the names corresponding to the country names (including capitalisations) given in note 1 (missing spaces where necessary). `Country` should be an intsance of enum using the numbering convention given in note 1. Show should add in the spaces again so `show GreatBritain = "Great Britain"`.

### Functions
```hs
neighbours :: Country -> [Country]
``` 
Given a country, returns the list of all neighbouring countries.
```hs
isNeighbour :: Country -> Country -> Bool
```
Given two countries, returns `True` if there is an edge connecting them and `False` otherwise.

## State 
### Types
```hs
GameState -- (Eq)
```

```hs
Player (Black | Blue | Green | Red | Yellow) -- (Eq, Show) 
```

```hs
MiniPhase (WonBattle Country Country Attackers | Normal) -- (Eq, Show)
```

```hs 
Phase (Reinforce | Attack MiniPhase | Fortify) -- (Eq, Show)
```

### Functions
```hs
newGame :: [Player] -> StdGen -> GameState
```
Creates a blank game with no troops in any country. Players should be given in turn order starting with the current player.

```hs
troops :: GameState -> Country -> Int
```
Gives the number of troops in a given country.

```hs
turnOrder :: GameState -> [Player]
```
Returns a list of players (in the order of play) starting with the current player.

```hs
owner :: GameState -> Country -> Player
```
Gives the player who owns a certain country.

```hs
changeTroops :: Country -> Int -> GameState -> GameState
```
Modifies (adds or subtracts) the given number of troops (to or from a country).

```hs
changeOwner :: Country -> Player -> GameState -> GameState
```
Changes the owner of a country.

```hs
nextTurn :: GameState -> GameState
```
Advances to the next turn (and updates the phase).

```hs
currentStdGen :: GameState -> StdGen
```
Gets the current StdGen.

```hs
updateStdGen :: StdGen -> GameState -> GameState
```
Replaces the current StdGen.

```hs
phase :: GameState -> Phase
```
Gets the current phase.

```hs
nextPhase :: GameState -> GameState
```
Updates the current phase.

## Battles
### Types
```hs
Defenders (OneDef | TwoDef) -- (Eq, Show, Ord, Enum)
```
Note that for `Enum`, enumeration starts at 1 not 0.

```hs
Attackers (OneAtt | TwoAtt | ThreeAtt) -- (Eq, Show, Ord, Enum)
```
Note that for `Enum`, enumeration starts at 1 not 0.

### Functions
```hs
doBattle :: Attackers -> Defenders -> StdGen -> (Int, Int, StdGen)
```
Takes the number of Attackers and the number of defenders and returns the outcome of the battle (attackers lost, defenders lost).

## Moves

```hs
reinforce :: [(Country, Int)] -> GameState -> Maybe GameState
```
Add reinforcements (specified as a list of pairs of country and non-negative integer number of troops to add). Must be during the correct phase.

```hs
fortify :: Country -> Country -> Int -> GameState -> Maybe GameState
```
Moves troops from one country to another. Countries must be neighbours and owned by the current player. Must be during the correct phase.


```hs
attack :: Attackers -> Defenders -> Country -> Country -> GameState -> Maybe GameState
```

```hs
invade :: Int -> GameState -> Maybe GameState
```

```hs
skipFortify :: GameState -> Maybe GameState
```

```hs
endAttack :: GameState -> Maybe GameState
```

# Notes 

## Note 1

The country names vary between different editions of Risk. These are the names we will use. They are each given a unique id between 0 and 41.

| Countries                | id |
|--------------------------|----|
| Alaska                   | 0  |
| Alberta                  | 1  |
| Central America          | 2  |
| Eastern United States    | 3  |
| Greenland                | 4  |
| Northwest Territory      | 5  |
| Ontario                  | 6  |
| Quebec                   | 7  |
| Western United States    | 8  |
| Argentina                | 9  |
| Brazil                   | 10 |
| Peru                     | 11 |
| Venezuela                | 12 |
| Great Britain            | 13 |
| Iceland                  | 14 |
| Northern Europe          | 15 |
| Scandinavia              | 16 |
| Southern Europe          | 17 |
| Ukraine                  | 18 |
| Western Europe           | 19 |
| Congo                    | 20 |
| East Africa              | 21 |
| Egypt                    | 22 |
| Madagascar               | 23 |
| North Africa             | 24 |
| South Africa             | 25 | 
| Afghanistan              | 26 |
| China                    | 27 |
| India                    | 28 |
| Irkutsk                  | 29 |
| Japan                    | 30 |
| Kamchatka                | 31 |
| Middle East              | 32 |
| Mongolia                 | 33 |
| Siam                     | 34 |
| Siberia                  | 35 |
| Ural                     | 36 |
| Yakutsk                  | 37 |
| Eastern Australia        | 38 |
| Indonesia                | 39 |
| New Guinea               | 40 |
| Western Australia        | 41 |
