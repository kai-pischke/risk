
# Modules
## Graph
### Types
```hs
Eq n => Graph n
```
This type models an undirected (not necessarily connected) graph. We consider the vertecies of the graph to be *nodes* (which are members of the type `n`). We require that each node is in a distinct equivelence class of the equivalence relation `Eq n`. It is importnat to note that we consider all graphs (of type `Graph n`) to contain every inhabitant of the type `n`. In general, we assume that all nodes are disconnected unless otherwise specified when creating the graph. So for example a `Graph Integer` contains an infinite number of nodes. We say that two nodes are neighbours if (and only if) there is an edge between those two vertecies of the graph.
### Functions
```hs
neighbours :: Eq n => Graph n -> n -> [n]
```  
Given a graph and a node (vertex of the graph), returns a list of the directly adjacent nodes. Note that a consequence of the fact that we consider all members of a type to be nodes is that the function `neighbors` is a total function, so defined for all possible n (returning the empty list for nodes with no neighbours). 
```hs
isNeighbour :: Eq n => Graph n -> n -> n -> Bool
```  
Given a graph and two nodes, returns `True` if there is an edge connecting them and `False` otherwise. Neighbours is also a total function.
```hs
makeGraph :: Eq n => [(n, [n])] -> Graph n
```
Given a valid adjacency list for an undirected graph, returns the corresponding representation as a `Graph n`. The adjacency list takes the form of a list of pairs. Each pair contains a member of the type `n` followed by a (possibly empty) list of neighbours. The (isNeighbour) relation must be symmetric (so if `a :: n` has `b :: n` in its list, then `b :: n` must have `a :: n` in its list). Any pairs containing the empty list as a second element are ignored - these nodes (as well as any others not included) are treated as having no neighbours. This well-formedness condition, together with the condition that no duplicate entries exist, ae precoditions to the function (which should throw an error if they are violated). 

## RiskMap

### Types
```hs
Country -- (Enum, Eq, Show, Ord, Bounded)
```
The `Country` type should be inhabited by 42 nullary constructors with the names corresponding to the country names (including capitalisations) given in note 1 (missing spaces where necessary). `Country` should be an intsance of enum using the numbering convention given in note 1. Show should add in the spaces again so `show GreatBritain = "Great Britain"`.

### Functions
```hs
neighbours :: Country -> [Country]
``` 
Given a country, returns the list of all neighbouring countries (in no specific order).
```hs
isNeighbour :: Country -> Country -> Bool
```
Given two countries, returns `True` if there is an edge connecting them and `False` otherwise.

## State 
### Types
```hs
GameState -- (Eq, Show)
```

```hs
Player (Black | Blue | Green | Red | Yellow) -- (Eq, Show) 
```

```hs
MiniPhase (WonBattle Country Country Attackers | Normal) -- (Eq, Show)
```

WonBattle take it's arguements as such: WonBattle (Attacking Country) (Defending Country) (Attackers Left After Attack)


```hs 
Phase (Reinforce | Attack MiniPhase | Fortify) -- (Eq, Show)
```

### Functions
```hs
newGame :: [Player] -> (Country -> (Player, Int)) -> StdGen -> GameState
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

```hs
updateMiniPhase :: MiniPhase -> GameState -> GameState
```
Does nothing if not in attack phase, otherwise sets phase to Attack MiniPhase (inserting the provided MiniPhase).

## SetupBoard
### Types
```hs
SetupBoardState -- (Eq, Show)
```
Keeps track of how many troops on each country, who owns each country as well as player order and number of troops remaining to be placed for each player. 
```hs 
SetupState (Incomplete SetupBoardState | Complete SetupBoardState) -- (Eq, Show)
```
Contains a SetupBoardState and information about 
### Functions
```hs
emptyBoard :: [Players] -> SetupState
```
Creates a blank board with no troops and where no countries are owned yet.

```hs
placeTroop :: Country -> SetupState -> SetupState
```
Partial function, only defined for incomplete `SetupState` and only when the current player owns the given country. 
```hs
completeBoardOwner :: SetupBoardState -> Country -> (Player, Int)
```
Partial function, only defined for complete `SetupState`, gives the owner and number of troops in each country.

## Battle
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
Add reinforcements (specified as a list of pairs of country and non-negative integer number of troops to add). Must be during the correct phase.  Should update phase to Attack Normal.

```hs
fortify :: Country -> Country -> Int -> GameState -> Maybe GameState
```
Moves troops from one country to another. Countries must be neighbours and owned by the current player. Must be during the correct phase. Troops are sent from the first country to the second one. Should update phase to Reincorce and call nextTurn.


```hs
attack :: Attackers -> Defenders -> Country -> Country -> GameState -> Maybe GameState
```
Attacks the 2nd Country from the first with the number of attackers and defenders specified. Must Be neighbouring countries owned by different players. Must be during the correct phase. If 2nd Country ends up with 0 troops must start a WonBattle MiniPhase, with the number of troops left after casualties from the attack.

```hs
invade :: Int -> GameState -> Maybe GameState
```
Invades a country from another with Int number of troops. Should be only able to be called in WonBattle MiniPhase. Invades defending country from attacking. Can't invade  with less than the number of attackers left. Should move back to a Normal Attack MiniPhase

```hs
skipFortify :: GameState -> Maybe GameState
```
Must be during the correct phase. Should update phase to Reincorce and call nextTurn.


```hs
endAttack :: GameState -> Maybe GameState
```
Must be during the Attack Normal MiniPhase. Should update phase to Fortify.


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
