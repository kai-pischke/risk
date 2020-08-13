module Server where
import Message (Response(..), Request(..), RequestType(..)) -- Alex should really export this
import Interface
import GameElements (Player)
import Parse
import Data.ByteString.Lazy (ByteString)
import qualified Network.WebSockets as WS
import System.Random (getStdGen)
import Control.Exception (finally)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (forM_, forever)
import Data.ByteString.Lazy.UTF8 as BLU

{- The State type holds all the current connections 
   and the current state of the Game.

   There will be a shared mutable variable of type MVar State
   that all the connection threads will be able to read and write to. -}

data State = State [(Player, WS.Connection)] Game

-- simply replaces the Game in a State with a new Game
updateGame :: Game -> State -> State
updateGame g (State ps _) = State ps g

-- updates the State (using addPlayer) and returns the new player's colour
connectNewPlayer :: WS.Connection -> MVar State -> IO Player
connectNewPlayer conn state = modifyMVar state $ \(State ps g) -> do
    let (p, g') = addPlayer g
    return (State ((p, conn):ps) g', p)

-- called when a player disconnects
disconnect :: Player -> IO()
disconnect player = putStrLn $ show player ++ " disconnected from the game."

-- send a message to a particular player 
send :: MVar State -> Player -> ByteString -> IO ()
send state player bytes = do
    State players _ <- readMVar state
    let (_, conn) = head $ filter ((==) player . fst) players
    WS.sendTextData conn bytes

-- deals with one cycle of receiving and then sending responses
respond :: WS.Connection -> MVar State -> Request -> IO ()
respond conn state req = do
    State _ game <- readMVar state
    let (response, game') = receive req game
    modifyMVar_ state (return . updateGame game')
    let bytesResp = encodeResponse response
    case response of
        General _ -> broadcast state bytesResp
        Special _ toPlayer -> send state toPlayer bytesResp
        Invalid _ toPlayer -> send state toPlayer bytesResp
        
-- prints information about a request
reqInfo :: Player -> RequestType -> String 
reqInfo p StartGame = show p ++ " started the game."
reqInfo p (PlaceTroop c) = show p ++ " placed a troop in " ++ show c ++ "."
reqInfo p (Attack c1 c2 _) = show p ++ " attacked " ++ show c2 ++ " from " ++ show c1 ++ "."
reqInfo p (Reinforce ts) = show p ++ " placed " ++ show (sum $ map snd ts) ++ " reinforcements."
reqInfo p (Fortify c1 c2 n) = undefined
reqInfo p (Invade c) = undefined
reqInfo p (ChooseDefenders d) = undefined
reqInfo p EndAttack = undefined
reqInfo p SkipFortify = undefined

-- deals with one cycle of receiving and then sending responses    
play :: WS.Connection -> Player -> MVar State -> IO ()
play conn player state = do
    -- send "joining" msg
    WS.sendTextData conn $ BLU.fromString ("{\"colour\":\"" ++ show player ++ "\"}")
    
    -- log the new player
    putStrLn $ "A new player joined and was assigned " ++ show player ++ "."
    
    -- deal with requests
    forever $ do        
        bytesReq <- WS.receiveData conn
        case decodeRequest bytesReq of 
            Left req -> do 
                let (Request p r) = req
                putStrLn $ reqInfo p r
                respond conn state req
            Right err -> 
                WS.sendTextData conn err

 -- send the same thing to every player  
broadcast :: MVar State -> ByteString -> IO ()
broadcast state msg = do
    State players _ <- readMVar state
    forM_ players $ \(_, conn) ->
        WS.sendTextData conn msg

-- main application run by the server  
application :: MVar State -> WS.ServerApp
application state pending = do
    
    -- accept a new connection
    conn <- WS.acceptRequest pending
    player <- connectNewPlayer conn state
    
    -- keep it alive by pinging every 30s
    WS.withPingThread conn 30 (return ()) $ do
    
        -- will cleanup after connection closed
        finally (play conn player state) (disconnect player)
        
    
run :: Int -> IO ()
run port = do
    -- create the game 
    gen <- getStdGen
    let game = empty gen
    
    -- store it in an MVar
    state <- newMVar $ State [] game
    
    -- run the main application
    WS.runServer "0.0.0.0" port $ application state