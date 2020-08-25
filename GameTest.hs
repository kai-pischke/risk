module GameTest where
import System.Random (getStdGen, StdGen)
import GameElements
import RiskBoard
import Interface
import Message 
import Data.Bifunctor
import Debug.Trace (trace)

countries = [(minBound::Country)..]

nPlayerGame :: Int -> StdGen -> ([Player], Game)
nPlayerGame 0 g = ([], empty g)
nPlayerGame n g = (p:ps, g'')
    where
    (ps, g') = nPlayerGame (n-1) g
    (p, g'') = addPlayer g'

-- also = (foldr (.) id $ replicate n (>>= first (:[]) . addPlayer)) ([], empty g)

manyRequests :: [Request] -> Game -> Maybe Game
manyRequests [] g = Just g
manyRequests (r:rs) g = maybe Nothing nextReq $ manyRequests rs g
    where 
    nextReq g' = let (resp, g'') = receive r g' in  
                 case resp of
                         (Invalid _ _) -> Nothing
                         _ -> Just g''
                        

toPartial :: [Player] -> Game -> Maybe Game 
toPartial ps = manyRequests $ zipWith (flip Request . PlaceTroop) countries ps
        
main :: IO()
main = do
    g <-  getStdGen
    let (players, start) = nPlayerGame 4 g -- 4 player game
    let (_, start') = Request (head players) StartGame `receive` start -- start the game
    let turnOrder = players ++ turnOrder -- infinite list of turnOrder
    let k = toPartial turnOrder start'
    let n = toPartial turnOrder start'
    print $ k