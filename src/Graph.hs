module Graph
    ( Graph,
      neighbors,
      isNeighbor,
      makeGraph
    ) where

data Graph n = Undefined

neighbors :: Eq n => Graph n -> n -> [n]
neighbors = undefined

isNeighbor :: Eq n => Graph n -> n -> n -> Bool
isNeighbor = undefined

makeGraph :: Eq n => [(n, [n])] -> Graph n
makeGraph = undefined
