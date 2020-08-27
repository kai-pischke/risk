module MovesSpec where

import Data.Maybe (fromJust)
import Test.Hspec
import Test.QuickCheck
import State
import Moves
import Battle
import System.Random
import GameElements

spec :: Spec

game = newGame [Red, Blue, Green] c (mkStdGen 0)

--What is this for?
--setup:: GameState -> GameState


c:: Country -> (Player, Int)
c WesternAustralia  = (Red, 3)
c EasternAustralia = (Blue, 3)
c NewGuinea = (Green, 100)
c Indonesia =  (Red, 3)
c Siam = (Red, 3)
c Brazil = (Blue, 3)
c Peru =  (Blue, 3)
c _ = (Black, 3)


spec = do
    describe "Fortify" $ do
        let testGame = (nextPhase.nextPhase) game

        context "Valid Inputs" $ do
            it "correctly adds troops to one country from another" $ do
                let t1g = (fromJust.(fortify WesternAustralia Indonesia 2)) testGame
                (troops t1g Indonesia == 5) `shouldBe` True
                (troops t1g WesternAustralia == 1) `shouldBe` True

            it "correctly adds troops to one country from another for another player" $ do
                let t1g = (fromJust.(fortify Peru Brazil 1).nextPhase.nextPhase.nextTurn) game
                (troops t1g Brazil == 4) `shouldBe` True
                (troops t1g Peru == 2) `shouldBe` True


        context "Invalid Inputs" $ do
            it "Doesn't let move more than num troops in country -1" $ do
                (fortify WesternAustralia Indonesia 3 testGame == Nothing) `shouldBe` True
                (fortify WesternAustralia Indonesia 1379 testGame == Nothing) `shouldBe` True

            it "Doesn't let fortify to or from a country not owned by cur player" $ do
                (fortify WesternAustralia EasternAustralia 2 testGame == Nothing) `shouldBe` True
                (fortify EasternAustralia WesternAustralia 2 testGame == Nothing) `shouldBe` True

            it "Doesn't let fortify to the same country" $ do
                (fortify WesternAustralia WesternAustralia 2 testGame == Nothing) `shouldBe` True


            it "Doesn't let another player fortify" $ do
                (fortify Brazil Peru 2 testGame == Nothing) `shouldBe` True


        context "Rule variations" $ do
            it  "Only allows for neighbouring countries" $ do
                (fortify Siam WesternAustralia 2 testGame == Nothing) `shouldBe` True

-- PLEASE FIX REINFORCE WITH NEW TYPE SIGNATURE --

{-
    describe "Reinforce" $ do
        let testGame = game
        context "Valid Inputs" $ do
            it "Reinforcing one country" $ do
                let t1g = (fromJust.(reinforce None [(WesternAustralia,3)])) testGame
                (troops t1g WesternAustralia == 6) `shouldBe` True

            it "Reinforcing multiple country" $ do
                let t1g = (fromJust.(reinforce None [(WesternAustralia,2), (Siam, 1)])) testGame
                (troops t1g WesternAustralia == 5) `shouldBe` True
                (troops t1g Siam == 4) `shouldBe` True

            it "Using Cards" $ do
                let t1g = (fromJust.(reinforce [Artillery, Artillery, Artillery] [(WesternAustralia,11)])) testGame
                (troops t1g WesternAustralia == 14) `shouldBe` True

            it "Picks the best option when using Cards" $ do
                let t1g = (fromJust.(reinforce [Wild, Wild, Infantry] [(WesternAustralia,13)])) testGame
                (troops t1g WesternAustralia == 16) `shouldBe` True

        context "Invalid Inputs" $ do
            it "Reinforcing a country owned by another Player" $ do
                (reinforce None [(EasternAustralia, 3)] testGame == Nothing) `shouldBe` True
                (reinforce None [(WesternAustralia, 1), (EasternAustralia, 2)] testGame == Nothing) `shouldBe` True

            it "Doesn't let reinforce with invalid number of troops" $ do
               (reinforce None [(WesternAustralia,4)] testGame == Nothing) `shouldBe` True
               (reinforce None [(WesternAustralia,6)] testGame == Nothing) `shouldBe` True

               (reinforce None [(WesternAustralia, 5), (Siam, 2)] testGame == Nothing) `shouldBe` True
               (reinforce None [(WesternAustralia, 2), (Siam, 2)] testGame == Nothing) `shouldBe` True

               (reinforce [Artillery, Artillery, Artillery] [(WesternAustralia, 5), (Siam, 2)] testGame == Nothing) `shouldBe` True
               (reinforce [Artillery, Artillery, Artillery] [(WesternAustralia, 21), (Siam, 2)] testGame == Nothing) `shouldBe` True

            it "Doesn't work with invalid cards" $ do
                (reinforce [Artillery, Cavalry, Artillery] [(WesternAustralia, 5), (Siam, 2)] testGame) `shouldBe` Nothing
                (reinforce [Cavalry, Cavalry, Artillery] [(WesternAustralia, 5), (Siam, 2)] testGame) `shouldBe` Nothing

-}


    describe "Attack" $ do
        let testGame = (nextPhase) game
        context "Valid Inputs" $ do
            it "Correctly leaves it in the correct MidBattle miniphase" $ do
                let t1g = (fromJust . attack WesternAustralia EasternAustralia TwoAtt) testGame
                (phase t1g) `shouldBe` (Attack $ MidBattle WesternAustralia EasternAustralia TwoAtt)
        context "Invalid Inputs" $ do
            it "Check it doesn't work in any other phase/miniphase" $ do
                ((attack WesternAustralia EasternAustralia OneAtt) game == Nothing) `shouldBe` True
                (((attack WesternAustralia EasternAustralia OneAtt).changeMiniPhase (WonBattle EasternAustralia WesternAustralia TwoAtt).nextPhase) game == Nothing) `shouldBe` True
                (((attack WesternAustralia EasternAustralia OneAtt).nextPhase.nextPhase) game == Nothing) `shouldBe` True

            it "Check it doesn't work if you try and attack with too many troops" $ do
                let t1g = changeTroops WesternAustralia (-2) testGame
                ((attack WesternAustralia EasternAustralia TwoAtt) t1g == Nothing) `shouldBe` True

            it "Doesn't let you attack with an invalid country pair" $ do
                ((attack WesternAustralia Peru OneAtt testGame == Nothing) `shouldBe` True)
                ((attack Siam Indonesia OneAtt testGame == Nothing) `shouldBe` True)
                ((attack EasternAustralia WesternAustralia OneAtt testGame == Nothing) `shouldBe` True)
                ((attack Peru Brazil OneAtt testGame == Nothing) `shouldBe` True)
                ((attack WesternAustralia WesternAustralia OneAtt testGame == Nothing) `shouldBe` True)
                ((attack EasternAustralia EasternAustralia OneAtt testGame == Nothing) `shouldBe` True)

    describe "Attack" $ do
        let testGame = (nextPhase) game
        context "Valid Inputs" $ do
            it "Correctly updates attackers,defenders and stdGen" $ do
                let t1g = (fromJust. chooseDefenders TwoDef. attack WesternAustralia EasternAustralia TwoAtt) testGame

                (troops t1g WesternAustralia == 2) `shouldBe` True
                (troops t1g EasternAustralia == 2) `shouldBe` True
                ((currentStdGen t1g) == mkStdGen 0) `shouldBe` False
                (phase t1g == Attack Normal) `shouldBe` True
            it "Correctly leaves game in WonBattle state if defenders wiped out" $ do
                let t1g = (fromJust. chooseDefenders OneDef. attack WesternAustralia EasternAustralia ThreeAtt . updateStdGen (mkStdGen 3).changeTroops WesternAustralia 1 .(changeTroops EasternAustralia (-2))) testGame
                (phase t1g == Attack (WonBattle WesternAustralia EasternAustralia ThreeAtt))

        context "Invalid Inputs" $ do
            it "Check it doesn't work in any other phase/miniphase" $ do
                ((chooseDefenders OneDef . attack WesternAustralia EasternAustralia OneAtt) game == Nothing) `shouldBe` True
                (((chooseDefenders OneDef . attack WesternAustralia EasternAustralia OneAtt).changeMiniPhase (WonBattle EasternAustralia WesternAustralia TwoAtt).nextPhase) game == Nothing) `shouldBe` True
                (((chooseDefenders OneDef . attack WesternAustralia EasternAustralia OneAtt).changeMiniPhase (MidBattle EasternAustralia WesternAustralia TwoAtt).nextPhase) game == Nothing) `shouldBe` True
                (((chooseDefenders OneDef . attack WesternAustralia EasternAustralia OneAtt).nextPhase.nextPhase) game == Nothing) `shouldBe` True

            it "Check it doesn't work if you try and defend with too many troops" $ do
                let t1g = changeTroops EasternAustralia (-2) testGame
                ((chooseDefenders TwoDef .attack WesternAustralia EasternAustralia TwoAtt) game == Nothing) `shouldBe` True

    describe "Invade" $ do
        let testGame = nextPhase game
        context "Valid Inputs" $ do
            it "Check it moves the troops correctly" $ do
                let t1g = (fromJust.invade 2.changeMiniPhase (WonBattle WesternAustralia EasternAustralia TwoAtt).changeTroops EasternAustralia (-3)) testGame
                (owner t1g EasternAustralia == Red) `shouldBe` True
                (troops t1g EasternAustralia == 2) `shouldBe` True
                (troops t1g WesternAustralia == 1) `shouldBe` True

            it "Check it moves the troops correctly on another players turn" $ do
                let t1g = (fromJust.invade 2.changeMiniPhase (WonBattle EasternAustralia WesternAustralia TwoAtt).(changeTroops WesternAustralia (-3)) .nextPhase.nextTurn) testGame
                (owner t1g WesternAustralia == Blue) `shouldBe` True
                (troops t1g EasternAustralia == 1) `shouldBe` True
                (troops t1g WesternAustralia == 2) `shouldBe` True
            it "Correctly kicks a player when they loose all their territory" $ do
                let t1g = (fromJust.invade 1. changeMiniPhase (WonBattle WesternAustralia NewGuinea OneAtt).(changeTroops NewGuinea (-100))) testGame
                (elem Green (turnOrder t1g)) `shouldBe` False

        context "Invalid Inputs" $ do
            it "Check it doesn't work in any other phase/miniphase" $ do
                (invade 2 game == Nothing) `shouldBe` True
                ((invade 2.nextPhase) game == Nothing) `shouldBe` True
                ((invade 2.nextPhase.nextPhase) game == Nothing) `shouldBe` True

            it "Check it doesn't let you invade with more or less troops than possible" $ do
                let testGame = (changeMiniPhase (WonBattle WesternAustralia EasternAustralia TwoAtt)) game
                (invade 0 testGame == Nothing) `shouldBe` True
                (invade 1 testGame == Nothing) `shouldBe` True
                (invade 4 testGame == Nothing) `shouldBe` True

            it "Check it erros with any invalid combination of countries" $ do
                ((print.invade 2.changeMiniPhase (WonBattle Siam Indonesia OneAtt)) testGame) `shouldThrow` anyException
                ((print.invade 2.changeMiniPhase (WonBattle EasternAustralia WesternAustralia OneAtt)) testGame) `shouldThrow` anyException
                ((print.invade 2.changeMiniPhase (WonBattle Peru Brazil OneAtt)) testGame) `shouldThrow` anyException
                ((print.invade 2.changeMiniPhase (WonBattle WesternAustralia Peru OneAtt)) testGame) `shouldThrow` anyException



    describe "skipFortify" $ do
        let testGame = (nextPhase.nextPhase) game
        context "Valid Inputs" $ do
            it "Check it skips the fortify stage" $ do
                let t1g = (fromJust.skipFortify) testGame
                (phase t1g == Reinforce `shouldBe` True)
                ((head.turnOrder) t1g == Blue `shouldBe` True)

        context "Invalid Inputs" $ do
            it "Check it doesn't work in any other phase" $ do
                (skipFortify game == Nothing) `shouldBe` True
                ((skipFortify.nextPhase) game == Nothing) `shouldBe` True


    describe "endAttack" $ do
        context "Valid Inputs" $ do
            it "Check it ends the Attack stage" $ do
                ((phase.fromJust.endAttack.nextPhase) game == Fortify `shouldBe` True)

        context "Invalid Inputs" $ do
            it "Check it doesn't work in any other phase" $ do
                (endAttack game == Nothing) `shouldBe` True
                ((endAttack.nextPhase.nextPhase) game == Nothing) `shouldBe` True
                ((endAttack.changeMiniPhase (WonBattle Brazil Peru OneAtt).nextPhase) game == Nothing) `shouldBe` True
