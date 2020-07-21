module Graph
    ( Graph,
      neighbors,
      isNeighbor,
      makeGraph
    ) where

-- A(n) (undirected) Graph is a list of verticies and a function to give verticies that a vertex connects too
data Graph n = Graph [n] (n -> [n])

neighbours :: Eq n => Graph n -> n -> [n]
neighbours (Graph _ f) = f

isNeighbour :: Eq n => Graph n -> n -> n -> Bool
isNeighbour (Graph _ f) c d = any (==d) (f c)


--Constructs a graph from an adjacency list; must be undirected Graph
makeGraph :: Eq n => [(n, [n])] -> Graph n
makeGraph ls
    |  (not.testReflexivity v) edge = error "Adjacency list Entered isn't reflexive"
    | otherwise = Graph v edge  
    where
        v = (map fst ls)
        edge = (makef ls)

-- Makes the function which maps an n to the list in the tuple with it
makef:: Eq n => [(n, [n])] -> n -> [n]
makef [] _ = []
makef (n:ns) x 
  | fst n == x = snd n
  | otherwise  = makef ns x

-- Tests that Ɐ A, B ∈ Dom f (A ∈ (f B) => B ∈ (f A)) 
testReflexivity:: Eq n => [n] -> (n -> [n]) -> Bool
testReflexivity [] f = True
testReflexivity (n:ns) f = all (\x -> any (==n) (f x))  (f n) && testReflexivity ns f 

main:: IO()
main = return()
