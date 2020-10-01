{-|
Module      : Server
Description : Main game server.
Maintainer  : Kai

This module packages the whole game into a single functin ('run') that creates a websockets-based server for the game.
-}
module Server 
    ( run
    ) where
import Message (RequestType(..)) -- Alex should really export this
import Interface 
import GameElements (Player)
import Parse
import qualified Network.WebSockets as WS
import System.Random (getStdGen)
import Control.Exception (finally)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (forM_, forever)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as BLU
import System.Directory
import System.FilePath
import Data.List
import Text.Read
import Data.Aeson (encode, decode)
import qualified Data.ByteString.Lazy as B

{- The State type holds all the current connections 
   and the current state of the Game.

   There will be a shared mutable variable of type MVar State
   that all the connection threads will be able to read and write to. -}

data State = State [(Player, WS.Connection)] Game

gamesFolderName :: FilePath
gamesFolderName = "risk" </> "games"

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
respond _ state req = do
    State _ game <- readMVar state
    let (response, game') = receive req game
    modifyMVar_ state (return . updateGame game')
    let bytesResp = encodeResponse response
    putStrLn $ debugMsg req response
    case response of
        General _ -> broadcast state bytesResp
        Special _ toPlayer -> send state toPlayer bytesResp
        Invalid _ toPlayer -> send state toPlayer bytesResp
        GameWon _ -> undefined

debugMsg :: Request -> Response -> String
debugMsg (Request p r) (General _) = "VALID REQUEST   >> " ++ reqInfo p r
debugMsg (Request p r) (Special _ _) = "VALID REQUEST   >> " ++ reqInfo p r
debugMsg _ (Invalid i _) = "INVALID REQUEST >> " ++ show i
debugMsg _ (GameWon _) = "GAME WON (not really implemented yet)"

-- prints information about a request
reqInfo :: Player -> RequestType -> String 
reqInfo p StartGame = show p ++ " started the game."
reqInfo p (PlaceTroop c) = show p ++ " placed a troop in " ++ show c ++ "."
reqInfo p (Attack c1 c2 _) = show p ++ " attacked " ++ show c2 ++ " from " ++ show c1 ++ "."
reqInfo p (Reinforce ti ts) = show p ++ " placed " ++ show (sum $ map snd ts) ++ " reinforcements using trade in:" ++ show ti ++ "."
reqInfo p (Fortify c1 c2 n) = show p ++ " fortified their position, moving " ++ show n ++ " troops from " ++ show c1 ++ " to " ++ show c2 ++ "."
reqInfo p (Invade c) = show p ++ " invaded " ++ show c ++ "."
reqInfo p (ChooseDefenders d) = show p ++ " chose to roll " ++ show (fromEnum d) ++ " dice."
reqInfo p EndAttack = show p ++ " ended the attack phase."
reqInfo p SkipFortify = show p ++ " skipped the fortify phase."
reqInfo p (Trade _ _) = show p ++ " did a trade."
reqInfo p SaveGame = show p ++ " wants to save the game."
reqInfo p (LoadGame i) = show p ++ " wants to load the game with id " ++ show i ++ "."

takeJusts :: [Maybe a] -> [a]
takeJusts [] = []
takeJusts ((Just x):xs) = x : takeJusts xs
takeJusts (Nothing:xs) = takeJusts xs

getGames :: IO([Int]) 
getGames = do 
    gamesFolder <- getAppUserDataDirectory gamesFolderName
    fileList <- getDirectoryContents gamesFolder
    let filtered = takeJusts
                 $ map (>>= readMaybe)  
                 $ map (stripPrefix "saved_game") 
                 $ map takeBaseName 
                 $ filter (isExtensionOf "json") fileList
    return filtered 
    
save :: MVar State -> IO ()
save s = do
    gamesFolder <- getAppUserDataDirectory gamesFolderName
    createDirectoryIfMissing True gamesFolder
    gs <- getGames
    let gameId = case gs of 
                    [] -> 0
                    _  -> maximum gs + 1
    State _ game <- readMVar s
    let encoded = encode game
    let saveLocation = (gamesFolder </> addExtension ("saved_game" ++ show gameId) "json")
    putStrLn $ "SAVED GAME      >> Location: " ++ saveLocation
    B.writeFile saveLocation encoded

load :: MVar State -> Int -> IO ()
load s gameId = do
    gamesFolder <- getAppUserDataDirectory gamesFolderName
    createDirectoryIfMissing True gamesFolder
    gs <- getGames
    let gameName = (gamesFolder </> addExtension ("saved_game" ++ show gameId) "json")
    if not (gameId `elem` gs) then return () else do
        rawGame <- B.readFile gameName
        let Just decodedGame = (decode rawGame) :: Maybe Game
        modifyMVar_ s $ \(State ps _) -> do
            return (State ps decodedGame)
        putStrLn $ "LOADED GAME     >> Location: " ++ gameName


-- deals with one cycle of receiving and then sending responses    
play :: WS.Connection -> Player -> MVar State -> IO ()
play conn player state = do
    -- send "joining" msg
    WS.sendTextData conn $ BLU.fromString ("{\"kind\": \"colour\", \"colour\":\"" ++ show player ++ "\"}")
    
    -- log the new player
    putStrLn $ "A new player joined and was assigned " ++ show player ++ "."
    
    -- deal with requests
    forever $ do        
        bytesReq <- WS.receiveData conn
        case decodeRequest bytesReq of 
            Left req -> case req of 
                (Request _ SaveGame) -> do
                    putStrLn $ "SERVER REQUEST  >> " ++ reqInfo player SaveGame
                    save state
                (Request _ (LoadGame i)) -> do
                    putStrLn $ "SERVER REQUEST  >> " ++ reqInfo player (LoadGame i)
                    load state i
                _ -> do
                    respond conn state req
            Right err -> do
                putStrLn "INVALID JSON    >> :("
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
        
-- |Runs the game server. Requires an 'Int' which specifies the port.
run :: Int -> IO ()
run port = do
    -- create the game 
    gen <- getStdGen
    let game = empty gen
    
    -- store it in an MVar
    state <- newMVar $ State [] game
    
    -- run the main application
    WS.runServer "0.0.0.0" port $ application state