module InterfaceSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
import Control.Monad (foldM)
import Control.Exception (evaluate, try, Exception, SomeException)
import System.Random (mkStdGen, StdGen)
import Data.Either (isLeft)
import Interface (Game, empty, addPlayer, receive)
import Message
import RiskBoard
import GameElements
import SetupBoard


data ShowableGen = ShowableGen StdGen Int deriving Eq

newtype Permutation a = Permutation [a]

data NewGame = NewGame Game [Player] ShowableGen deriving Eq

data NewPartial = NewPartial Game [Player] [Country] ShowableGen | NewPartialError deriving Eq

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
        let result = maybeRequests (zipWith (flip Request . PlaceTroop) cs (cycle ps)) g
        case result of
            Just (General (Setup (PartiallyComplete s)), g') -> return $ NewPartial g' ps cs sg
            _ -> return NewPartialError
        
instance Arbitrary NewGame where 
    arbitrary = do
        sg@(ShowableGen g _) <- (arbitrary :: Gen ShowableGen)
        n <- elements [2..5]
        let (players@(p:_), game) = nPlayerGame g n
        let game' = snd $ receive (Request p StartGame) game
        return $ NewGame game' players sg

instance Show NewPartial where 
    show (NewPartial g ps cs sg) = "NewPartial " ++ show ps ++ " " ++ show cs ++ " " ++ show sg

instance Show NewGame where 
    show (NewGame g ps sg) = "NewGame " ++ show ps ++ " " ++ show sg
            
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
nPlayerGame g n = (p:ps, g'')
    where
    (ps, g') = nPlayerGame g (n-1)
    (p, g'') = addPlayer g'

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = not (x `elem` xs) && distinct xs

isEmptyBoard :: SetupState -> Bool
isEmptyBoard s = all (==(Nothing, 0)) $ map (incompleteBoardOwner s) countries

maybeRequests :: [Request] -> Game -> Maybe (Response, Game)
maybeRequests rs g = foldM modRec (error "no request", g) rs
 
modRec :: (Response, Game) -> Request -> Maybe (Response, Game)
modRec (_ , g') r = let (resp, g'') = receive r g' 
                     in case resp of 
                        (Invalid _ _) -> Nothing
                        _ -> Just (resp, g'')
    
                        
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
                it "should throw an error" $
                    pendingWith "test not implemented yet"

        describe "Receive" $ do
            describe "StartGame" $ do
                context "using valid inputs" $ do
                    it "should start the game correctly with all countries empty" $ property $
                        \sg@(ShowableGen g _) -> forAll (elements [2..5]) 
                        $ \n -> label ("using " ++ show n ++ " players") $
                                let (players@(p:_), game) = nPlayerGame g n
                                in let resp = fst $ receive (Request p StartGame) game
                                in case resp of 
                                    General (Setup s@(Incomplete _)) -> isEmptyBoard s
                                    _ -> False
                context "using invalid inputs" $ do
                    it "should throw an error in other phases" $ do
                        pendingWith "test not implemented yet"
                    it "should throw an error if called with only 0-1 players" $ do
                        pendingWith "test not implemented yet"

            describe "PlaceTroop" $ do     
                context "during the Incomplete subphase" $ do
                    it "places troops correctly for valid input" $ property $ \game ->
                        game /= NewPartialError
                    it "does not allow players to place troops in occupied countries" $ property $ do
                        NewGame g ps sg <- (arbitrary :: Gen NewGame)
                        cs <- (arbitrary :: Gen [Country])
                        let result = maybeRequests (zipWith (flip Request . PlaceTroop) cs (cycle ps)) g
                        return $ (not $ distinct cs) ==> (result === Nothing)
                    it "enforces the turn order" $ property $ do
                        NewGame g ps sg <- (arbitrary :: Gen NewGame)
                        Permutation cs <- (arbitrary :: Gen (Permutation Country))
                        order <- vectorOf (length cs) (elements ps)
                        let result = maybeRequests (zipWith (flip Request . PlaceTroop) cs order) g
                        return $ (not $ and $ zipWith (==) order (cycle ps)) ==> (result === Nothing)
                context "during the PartiallyComplete subphase" $ do
                    it "places troops correctly for valid input" $ do
                        pendingWith "test not implemented yet"
                    it "does not allow players to place troops in foreign countries" $ do
                        pendingWith "test not implemented yet"
                    it "enforces troop limits" $ do
                        pendingWith "test not implemented yet"
                    it "enforces the turn order" $ do
                        pendingWith "test not implemented yet"
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