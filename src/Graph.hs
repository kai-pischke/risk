{-|
Module      : Graph
Description : Polymorphic undirected graph types.
Maintainer  : River

The basic implementation of undirected graphs that forms the basis for the underlying representation of our Risk board.
-}
module Graph
    ( Graph,
      neighbours,
      isNeighbour,
      makeGraph
    ) where

--- EXTERNAL --- 

-- |This type models an undirected (not necessarily connected) graph. 
-- We consider the vertecies of the graph to be __nodes__ (which are members of the type @n@). 
-- We require that each node is in a distinct equivelence class of the equivalence relation @Eq n@. 
-- It is important to note that we consider all graphs (of type @'Graph' n@) to contain every inhabitant of the type @n@. 
-- In general, we assume that all nodes are disconnected unless otherwise specified when creating the graph. 
-- So for example a @'Graph' Integer@ contains an infinite number of nodes. 
-- We say that two nodes are neighbours if (and only if) there is an edge between those two vertecies of the graph.
data Graph n = Graph [n] (n -> [n])

-- |Given a graph and a node (vertex of the graph), returns a list of the directly adjacent nodes. 
-- Note that, as a consequence of the fact that we consider all members of a type to be nodes, 
-- the function 'neighbours' is a total function, so defined for all possible n 
-- (returning the empty list for nodes with no neighbours).
neighbours :: Eq n => Graph n -> n -> [n]
neighbours (Graph _ f) = f

-- |Given a graph and two nodes, returns @True@ if there is an edge connecting them and @False@ otherwise. 
-- 'isNeighbour' is also a total function (see 'neighbours').
isNeighbour :: Eq n => Graph n -> n -> n -> Bool
isNeighbour g c d = any (==d) (neighbours g c)


-- |Given a valid adjacency list for an undirected graph, returns the corresponding representation as a @'Graph' n@. 
-- The adjacency list takes the form of a list of pairs. 
-- Each pair contains a member of the type n followed by a (possibly empty) list of neighbours. 
-- The ('isNeighbour') relation must be symmetric:
-- (so if @a, b :: n@ if @a@ has @b@ in its list, then @b@ must have @a@ in its list). 
-- Any pairs containing the empty list as a second element are ignored 
-- - these nodes (as well as any others not included) are treated as having no neighbours. 
-- This well-formedness condition, together with the condition that no duplicate entries exist, 
-- as precoditions to the function (which should throw an error if they are violated).
makeGraph :: Eq n => [(n, [n])] -> Graph n
makeGraph ls
    |  (not.testReflexivity v) edge = error "Adjacency list Entered isn't reflexive"
    | otherwise = Graph v edge  
    where
        v = (map fst ls)
        edge = (makef ls)

--- INTERNAL --- 

-- Makes the function which maps an n to the list in the tuple with it
makef :: Eq n => [(n, [n])] -> n -> [n]
makef [] _ = []
makef (n:ns) x 
  | fst n == x = snd n
  | otherwise  = makef ns x

-- Tests that Ɐ A, B ∈ Dom f (A ∈ (f B) => B ∈ (f A)) 
testReflexivity:: Eq n => [n] -> (n -> [n]) -> Bool
testReflexivity [] _ = True
testReflexivity (n:ns) f = all (\x -> any (==n) (f x))  (f n) && testReflexivity ns f 
