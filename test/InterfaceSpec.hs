module InterfaceSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Monadic
import Control.Exception (evaluate, try, Exception, SomeException)
import System.Random (mkStdGen, StdGen)
import Data.Either (isLeft)
import Interface (Game, empty, addPlayer, receive)
import Message
import RiskBoard
import GameElements


data ShowableGen = ShowableGen StdGen Int
newtype Permutation a = Permutation [a]

instance Arbitrary ShowableGen where 
    arbitrary = do
        s <- (arbitrary :: Gen Int)
        return $ ShowableGen (mkStdGen s) s

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
                it "should throw an error" $ property $ \sg@(ShowableGen g _) ->
                    pendingWith "test not implemented yet"

        describe "Receive" $ do
            describe "StartGame" $ do
                context "using valid inputs" $ do
                    it "should start the game correctly" $ do
                        pendingWith "test not implemented yet"
                context "using invalid inputs" $ do
                    it "should throw an error in other phases" $ do
                        pendingWith "test not implemented yet"
                    it "should throw an error if called with only 0-1 players" $ do
                        pendingWith "test not implemented yet"

            describe "PlaceTroop" $ do     
                context "during the Incomplete subphase" $ do
                    it "places troops correctly for valid input" $ do
                        pendingWith "test not implemented yet"
                    it "does not allow players to place troops in occupied countries" $ do
                        pendingWith "test not implemented yet"
                    it "enforces the turn order" $ do
                        pendingWith "test not implemented yet"
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