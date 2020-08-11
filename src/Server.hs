{-# LANGUAGE OverloadedStrings #-}
module Server where
import Message (Response(..), Request(..)) -- Alex should really export this
import Interface
import GameElements (Player)
import Parse
import Data.ByteString.Lazy (ByteString)
import qualified Network.WebSockets as WS
import System.Random (getStdGen)
import Control.Exception (finally)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (forM_)

data State = State [(Player, WS.Connection)] Game


updateGame :: Game -> State -> State
updateGame g (State ps _) = State ps g

connectNewPlayer :: WS.Connection -> MVar State -> IO Player
connectNewPlayer conn state = modifyMVar state $ \(State ps g) -> do
    let (g, p) = addPlayer g
    return (State ((p, conn):ps) g, p)
    
disconnect :: IO()
disconnect = return ()

respond :: WS.Connection -> MVar State -> Request -> IO ()
respond conn state req = return ()
    
play :: WS.Connection -> Player -> MVar State -> IO ()
play conn player state = do
    -- send "joining" msg
    WS.sendTextData conn $ ("you are in the game!" :: ByteString)
    
    -- listen
    bytesReq <- WS.receiveData conn
    case decodeRequest bytesReq of 
        Left req -> respond conn state req 
        Right err -> WS.sendTextData conn err

broadcast :: MVar State -> ByteString -> IO ()
broadcast state msg = do
    State players _ <- readMVar state
    forM_ players $ \(_, conn) ->
        WS.sendTextData conn msg

application :: MVar State -> WS.ServerApp
application state pending = do

    -- accept a new connection
    conn <- WS.acceptRequest pending
    player <- connectNewPlayer conn state
    
    -- keep it alive by pinging every 30s
    WS.withPingThread conn 30 (return ()) $ do
    
        -- will cleanup after connection closed
        finally (play conn player state) disconnect
        
    
run :: IO ()
run = do
    -- create the game 
    gen <- getStdGen
    let game = empty gen
    
    -- store it in an MVar
    state <- newMVar $ State [] game
    
    -- run the main application
    WS.runServer "0.0.0.0" 9160 $ application state