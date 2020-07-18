
## Graph

### Functions
```hs
neighbors :: Graph n -> n -> [n]
```  
Given a graph and a node, returns a list of the directly adjacent nodes.
```hs
isNeighbor :: Graph n -> n -> n -> Bool
```  
Given a graph and two nodes, returns `True` if there is an edge connecting them and `False` otherwise.
```hs
makeGraph :: Eq n => [(n, [n])] -> Graph n
```
Given a valid adjacency list for an undirected graph, returns the corresponding representation as a `Graph n`.

## RiskMap

### Types
```hs
Country -- (Eq, Showable)
```
### Functions
```hs
neighbors :: Country -> [Country]
``` 
Given a country, returns the list of all neighbouring countries.
```hs
isNeighbor :: Country -> Country -> Bool
```
Given two countries, returns `True` if there is an edge connecting them and `False` otherwise.
