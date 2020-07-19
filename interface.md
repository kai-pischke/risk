
# Modules
## Graph
### Types
```hs
Eq n => Graph n
```

### Functions
```hs
neighbors :: Eq n => Graph n -> n -> [n]
```  
Given a graph and a node, returns a list of the directly adjacent nodes.
```hs
isNeighbor :: Eq n => Graph n -> n -> n -> Bool
```  
Given a graph and two nodes, returns `True` if there is an edge connecting them and `False` otherwise.
```hs
makeGraph :: Eq n => [(n, [n])] -> Graph n
```
Given a valid adjacency list for an undirected graph, returns the corresponding representation as a `Graph n`.

## RiskMap

### Types
```hs
Country -- (Enum, Eq, Showable)
```
The `Country` type should be inhabited by 42 nullary constructors with the names corresponding to the country names (including capitalisations) given in note 1 (missing spaces where necessary). `Country` should be an intsance of enum using the numbering convention given in note 1. Show should add in the spaces again so `show GreatBritain = "Great Britain"`.

### Functions
```hs
neighbors :: Country -> [Country]
``` 
Given a country, returns the list of all neighbouring countries.
```hs
isNeighbor :: Country -> Country -> Bool
```
Given two countries, returns `True` if there is an edge connecting them and `False` otherwise.

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
