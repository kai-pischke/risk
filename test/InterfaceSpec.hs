module InterfaceSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Gen.Unsafe (promote)
import Test.QuickCheck.Monadic
import Control.Monad (foldM)
import Control.Exception (evaluate, try, Exception, SomeException)
import System.Random (mkStdGen, StdGen)
import Data.Either (isLeft, isRight, either)
import Data.Maybe (isJust, fromJust, isNothing)
import Interface (Game, empty, addPlayer, receive)
import Message
import RiskBoard
import GameElements
import SetupBoard
import Debug.Trace (trace)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson 

data ShowableGen = ShowableGen StdGen Int deriving Eq

newtype Permutation a = Permutation [a]

data NewGame = NewGame Game [Player] ShowableGen deriving Eq

data NewPartial = NewPartial Game [Player] [Country] ShowableGen | NewPartialError (Request, Response) deriving Eq

data NewComplete = NewComplete Game [Player] [Country] ShowableGen | NewCompleteError (Request, Response) deriving Eq

initialTroops :: Int -> Int
initialTroops 6 = 20
initialTroops 5 = 25
initialTroops 4 = 30
initialTroops 3 = 35
initialTroops 2 = 40
initialTroops n = error $ "Game rules only specified for 2-6 players. Not sure what to do for "
      ++ show n ++ " players."
      
instance Arbitrary ShowableGen where 
    arbitrary = do
        s <- (arbitrary :: Gen Int)
        return $ ShowableGen (mkStdGen s) s

instance Arbitrary Country where 
    arbitrary = do 
        c <- elements [minBound..]
        return c
        
instance Arbitrary NewPartial where 
    arbitrary = do
        NewGame g ps sg <- (arbitrary :: Gen NewGame)
        Permutation cs <- (arbitrary :: Gen (Permutation Country))
        let result = doRequests (zipWith (flip Request . PlaceTroop) cs (cycle ps)) g
        case result of
            Left (General _, g') -> return $ NewPartial g' ps cs sg
            Right r -> return $ NewPartialError r

instance Arbitrary NewComplete where 
  arbitrary = do
    p <- (arbitrary :: Gen NewPartial)
    case p of 
        NewPartialError r -> return $ NewCompleteError r
        NewPartial g ps cs sg -> do
          let n = length ps 
          let owned = M.fromListWith (++) $ zip (cycle ps) (map (:[]) cs)
          let rounds = initialTroops n * n - length cs
          let order = take rounds $ drop (length cs) (cycle ps)
          choices <- promote $ map (elements . (owned M.!)) order
          case doRequests (zipWith (flip Request . PlaceTroop) choices order) g of 
            Left (_, g') -> return $ NewComplete g' ps (cs++choices) sg
            Right r -> return $ NewCompleteError r

        
instance Arbitrary NewGame where 
    arbitrary = do
        sg@(ShowableGen g _) <- (arbitrary :: Gen ShowableGen)
        n <- elements [2..5]
        let (players@(p:_), game) = nPlayerGame g n
        let game' = snd $ receive (Request p StartGame) game
        return $ NewGame game' players sg

instance Show NewPartial where 
    show (NewPartial g ps cs sg) = "NewPartial " ++ show ps ++ " " ++ show cs ++ " " ++ show sg
    show (NewPartialError (req,resp)) = "sent request: " ++ show req ++ " but received error: " ++ show resp
    
instance Show NewGame where 
    show (NewGame g ps sg) = "NewGame " ++ show ps ++ " " ++ show sg

instance Show NewComplete where 
  show (NewComplete g ps cs sg) = "NewPartial " ++ show ps ++ " " ++ show cs ++ " " ++ show sg
  show (NewCompleteError (req,resp)) = "sent request: " ++ show req ++ " but received error: " ++ show resp
      
instance Show a => Show (Permutation a) where
    show (Permutation s) = show s
    
instance Show ShowableGen where 
    show (ShowableGen _ s) = "(mkStdGen " ++ show s ++ ")"

instance (Enum a, Bounded a) => Arbitrary (Permutation a) where
    arbitrary = do 
        let xs = [minBound..]
        xs' <- shuffle xs
        return $ Permutation xs' 
                
throwsException :: a -> Property
throwsException = runPropIO . fmap (isLeft ::  Either SomeException a -> Bool) . try . evaluate

runPropIO :: Testable a => IO a -> Property
runPropIO = monadicIO . run

countries = [(minBound :: Country)..]

nPlayerGame :: StdGen -> Int -> ([Player], Game)
nPlayerGame g 0 = ([], empty g)
nPlayerGame g n = (ps++[p], g'')
    where
    (ps, g') = nPlayerGame g (n-1)
    (p, g'') = addPlayer g'

find :: Eq a => a -> [a] -> Maybe Int 
find _ [] = Nothing
find x (y:ys) = if x == y then Just 0 else fmap (1+) $ find x ys

firstdup :: Eq a => [a] -> Maybe Int
firstdup [] = Nothing
firstdup (x:xs) =  fmap (1+) (find x xs `combine` firstdup xs)
    where combine Nothing Nothing = Nothing
          combine (Just n) Nothing = Just n
          combine Nothing (Just n) = Just n
          combine (Just n) (Just m) = Just $ min n m
distinct :: Eq a => [a] -> Bool
distinct = isNothing . firstdup

isEmptyBoard :: SetupState -> Bool
isEmptyBoard s = all (==(Nothing, 0)) $ map (incompleteBoardOwner s) countries
    

doRequests :: [Request] -> Game -> Either (Response, Game) (Request, Response)
doRequests = foldl nextReq (\g -> Left (error "no request", g))             
    where 
        nextReq ::  (Game -> Either (Response, Game) (Request, Response)) -> Request -> Game -> Either (Response, Game) (Request, Response)
        nextReq f r = either (format r . receive r . snd) Right . f

        format :: Request -> (Response, Game) -> Either (Response, Game) (Request, Response)
        format req (resp@(Invalid _ _), _) = Right (req, resp)
        format _ anythingelse = Left anythingelse

ownerFromLists :: [Country] -> [Player] -> Country -> Player
ownerFromLists [] _ _ = error "country not in list"
ownerFromLists (c:cs) (p:ps) country = if country == c then p else ownerFromLists cs (ps ++ [p]) country


spec :: Spec
spec = do
        describe "addPlayer" $ do
            context "adding 2-5 players" $ do
                it "should give unique colours" $ property $ \sg@(ShowableGen g _) ->
                   forAll (elements [2..5]) -- valid number of players (2 to 5)
                   $ \n -> label ("using " ++ show n ++ " players") $
                           counterexample ("New game with " ++ show n ++ " players using " ++ show sg) $
                           let (players, game) = nPlayerGame g n 
                           in distinct players
                
            context "using >= 6 players" $ do
                it "should throw an error" $ property $ \sg@(ShowableGen g _) ->
                     forAll (elements [6..15]) -- invalid number of players (>= 6)
                     $ \n -> label ("using " ++ show n ++ " players") $
                             counterexample ("New game with " ++ show n ++ " players using " ++ show sg) $ 
                             throwsException $ snd $ nPlayerGame g n
        
            context "using a non-WaitingRoom" $ do
                it "should throw an error for Incomplete" $ property $ \(NewGame g ps sg) ->
                    label ("using " ++ show (length ps) ++ " players") $ throwsException $ addPlayer g
                it "should throw an error for PartiallyComplete" $ property $ \(NewPartial g ps cs sg) ->
                  label ("using " ++ show (length ps) ++ " players") $ throwsException $ addPlayer g
                it "should throw an error for Complete" $ property $ \(NewComplete g ps cs sg) ->
                  label ("using " ++ show (length ps) ++ " players") $ throwsException $ addPlayer g
        describe "Receive" $ do
            describe "StartGame" $ do
                context "using valid inputs" $ do
                    it "should start the game correctly with all countries empty" $ property $
                        \sg@(ShowableGen g _) -> forAll (elements [2..5]) 
                        $ \n -> label ("using " ++ show n ++ " players") $
                                let (players@(p:_), game) = nPlayerGame g n
                                in let resp = fst $ receive (Request p StartGame) game
                                in case resp of 
                                    General (Setup s@(Incomplete _)) -> isEmptyBoard s -- BAD
                                    _ -> False
                context "using invalid inputs" $ do
                    it "should throw an error for Incomplete" $ property $ \(NewGame g ps@(p:_) sg) ->
                        label ("using " ++ show (length ps) ++ " players") ((fst $ receive (Request p StartGame) g) === Invalid NotInWaitingRoom p)
                    it "should throw an error for PartiallyComplete" $ property $ \(NewPartial g ps@(p:_) cs sg) ->
                      label ("using " ++ show (length ps) ++ " players") ((fst $ receive (Request p StartGame) g) === Invalid NotInWaitingRoom p)
                    it "should throw an error for Complete" $ property $ \(NewComplete g ps@(p:_) cs sg) ->
                      label ("using " ++ show (length ps) ++ " players") ((fst $ receive (Request p StartGame) g) === Invalid NotInWaitingRoom p)
                    it "should throw an error if called with only 1 player" $ property $ \sg@(ShowableGen g _) -> 
                      let (players@(p:_), game) = nPlayerGame g 1
                      in let resp = fst $ receive (Request p StartGame) game
                      in resp === Invalid NotEnoughPlayers p

            describe "PlaceTroop" $ do     
                context "during the Incomplete subphase" $ do
                    it "places troops correctly for valid input" $ property $ \game ->
                        case game of 
                             NewPartialError r -> False
                             _ -> True
                    it "does not allow players to place troops in occupied countries" $ property $ do
                        NewGame g ps sg <- (arbitrary :: Gen NewGame)
                        cs <- (arbitrary :: Gen [Country])
                        let n = length ps
                        let reqs = zipWith (flip Request . PlaceTroop) cs (cycle ps)
                        let result = doRequests reqs g
                        let maybebadpos = fmap (`mod` n) (firstdup cs) 
                        return $ (not $ distinct cs) ==> let badcolour = ps !! (fromJust maybebadpos) in counterexample 
                            ("--- In a game with " ++ show ps ++ " made using " ++ show sg ++ " ---\n"
                            ++ "\nhere is a list of all the requests I sent:\n" ++ show reqs 
                            ++ "\n\nThere should be an Invalid InvalidMove since " ++ show badcolour 
                            ++ " tried to place a troop in an already occupied country. Here is some more info:\n"
                            ++ "\nthis is the bad (request, response) pair: " ++ show result 
                            ++ "\nexpected something like: Right (blah blah..., Invalid InvalidMove " ++ show badcolour ++ ")")  
                            $ case result of
                                 Right (_, Invalid InvalidMove b) -> b == badcolour
                                 _ -> False
                    it "enforces the turn order" $ property $ do
                        NewGame g ps sg <- (arbitrary :: Gen NewGame)
                        let n = length ps
                        Permutation cs <- (arbitrary :: Gen (Permutation Country))
                        turns <- chooseInt(1, length cs - 1)
                        wrong <- elements (take (n-1) $ drop (turns+1) (cycle ps))
                        let result = doRequests (zipWith (flip Request . PlaceTroop) cs (take turns (cycle ps))) g
                        case result of
                            Left (_, g') -> return $ counterexample ("It was actually meant to be " 
                                         ++ show (cycle ps !! turns) ++ "'s turn, so I was expecting \"Invalid NotYourTurn " 
                                         ++ show wrong ++ "\" when " ++ show wrong  ++ " tries to place troops.\n") 
                                         $ fst (receive (Request wrong (PlaceTroop (cs !! turns))) g') === Invalid NotYourTurn wrong
                            Right _ -> return $ counterexample "Failed before we even got to the test case (fix other tests first)" False
                context "during the PartiallyComplete subphase" $ do
                    it "places troops correctly for valid input" $ property $ \game ->
                          case game of 
                               NewCompleteError r -> property False
                               NewComplete _ ps _ _ -> label ("using " ++ show (length ps) ++ " players") True
                    it "does not allow players to place troops in foreign countries" $ do 
                        pendingWith "test not implemented yet"
                    it "enforces troop limits" $ do
                        pendingWith "test not implemented yet"
                    it "enforces the turn order" $ do
                        pendingWith "test not implemented yet"
                context "doing json encoding/decoding" $ do
                    it "obeys: decode . encode === id" $ property $ \game ->
                          case game of 
                                NewPartialError r -> property False
                                NewPartial g ps cs sg-> label ("using " ++ show (length ps) ++ " players") 
                                                      $ counterexample ("\nHere is how it was encoded:\n" ++ show (encode g) ++ "\n\n")
                                                      $ (decode . encode) g === Just g
                    it "decode . encode === id" $ property $ \game ->
                          case game of 
                                NewCompleteError r -> property False
                                NewComplete g ps cs sg-> label ("using " ++ show (length ps) ++ " players") 
                                                      $ counterexample ("\nHere is how it was encoded:\n" ++ show (encode g) ++ "\n\n")
                                                      $ (decode . encode) g === Just g
            {-
        describe "Attack" $ do
            context "Valid Inputs" $ do
                pendingWith ("Needs to work")
            context "Invalid Inputs" $ do
                pendingWith ("Needs to work")
                

        describe "Reinforce" $ do
          context "Valid Inputs" $ do
            pendingWith ("Needs to work")
          context "Invalid Inputs" $ do
            pendingWith ("Needs to work")
        describe "Fortify" $ do
            context "Valid Inputs" $ do
                pendingWith "Needs to work"
            context "Invalid Inputs" $ do
                pendingWith "Needs to work"
                
        describe "Invade" $ do
            context "Valid Inputs" $ do
                pendingWith "Needs to work"
            context "Invalid Inputs" $ do
                pendingWith "Needs to work"
                
        describe "ChooseDefenders" $ do
            context "Valid Inputs" $ do
                pendingWith "Needs to work"
            context "Invalid Inputs" $ do
                pendingWith "Needs to work"
                
        describe "EndAttack" $ do
            context "Valid Inputs" $ do
                pendingWith "Needs to work"
            context "Invalid Inputs" $ do
                pendingWith "Needs to work"
                
        describe "SkipFortify" $ do
            context "Valid Inputs" $ do
                pendingWith "Needs to work"
            context "Invalid Inputs" $ do
                pendingWith "Needs to work"
        -}