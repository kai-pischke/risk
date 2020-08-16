module InterfaceSpec where

import Test.Hspec
import Interface (Game, empty, addPlayer, receive)
import Message
import RiskBoard
import GameElements
import System.Random (mkStdGen)

makeSetup :: Game -> Game
makeSetup = snd.receive (Request Black StartGame) .f.f
    where f = fst.addPlayer

completeBoard:: Int -> [Country] -> [Player] -> Game -> Game
completeBoard 0 _ _ g = g
completeBoard n (c:cs) (p:ps) g = completeBoard (n-1) (cs++[c]) (ps++[p]) $ snd $ receive (Request p (PlaceTroop c)) g


makePlay :: Int -> [Player]-> Game -> Game
makePlay n ps = completeBoard n [minBound ::Country ..] ps . makeSetup


spec :: Spec
spec = do
    describe "addPlayer" $ do
        let testGame = empty (mkStdGen 0)
        context "Valid Inputs" $ do
            it "Should add in a player to the game" $ do
                (snd.addPlayer) testGame `shouldBe` Black
                (snd.addPlayer.fst.addPlayer) testGame `shouldBe` Blue

        context "Invalid Inputs" $ do
            it "Should error if try to add too many players" $ do
                ((print.addPlayer.fst.addPlayer.fst.addPlayer.fst.addPlayer.
                                  fst.addPlayer.fst.addPlayer.fst.addPlayer) testGame) `shouldThrow` anyException

            it "Should error if try to addPlayer not to a WaitingRoom" $ do
                ((print.addPlayer.makeSetup) testGame) `shouldThrow` anyException
                ((print.addPlayer.makePlay 80 [Black,Blue]) testGame) `shouldThrow` anyException

    describe "Receive" $ do
        describe "StartGame" $ do
            context "Valid Inputs" $ do
                it "Should start the game correctly" $ do
                    pending
            context "Invalid Inputs" $ do
                it "Doesn't work with too many Players" $ do
                    pending
                it "Doesn't work in other phases" $ do
                    pending

        describe "PlaceTroop" $ do
            context "Valid Inputs" $ do
                pending

            context "Invalid Inputs" $ do
                pending
        describe "Attack" $ do
            context "Valid Inputs" $ do
                pending

            context "Invalid Inputs" $ do
                pending
        describe "Reinforce" $ do
            context "Valid Inputs" $ do
                pending

            context "Invalid Inputs" $ do
                pending
        describe "Fortify" $ do
            context "Valid Inputs" $ do
                pending

            context "Invalid Inputs" $ do
                pending
        describe "Invade" $ do
            context "Valid Inputs" $ do
                pending

            context "Invalid Inputs" $ do
                pending
        describe "ChooseDefenders" $ do
            context "Valid Inputs" $ do
                pending

            context "Invalid Inputs" $ do
                pending
        describe "EndAttack" $ do
            context "Valid Inputs" $ do
                pending

            context "Invalid Inputs" $ do
                pending
        describe "SkipFortify" $ do
            context "Valid Inputs" $ do
                pending

            context "Invalid Inputs" $ do
                pending
