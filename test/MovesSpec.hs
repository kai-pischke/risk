module MovesSpec where

import Data.Maybe (fromJust)
import Test.Hspec
import Test.QuickCheck
import State
import Moves
import Battle
import System.Random


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


    describe "Reinforce" $ do
        let testGame = game
        context "Valid Inputs" $ do
            it "Reinforcing one country" $ do
                let t1g = (fromJust.(reinforce [(WesternAustralia,5)])) testGame
                (troops t1g WesternAustralia == 8) `shouldBe` True

            it "Reinforcing multiple country" $ do
                let t1g = (fromJust.(reinforce [(WesternAustralia,2), (Siam, 3)])) testGame
                (troops t1g WesternAustralia == 5) `shouldBe` True
                (troops t1g Siam == 6) `shouldBe` True

        context "Invalid Inputs" $ do
            it "Reinforcing a country owned by another Player" $ do
                (reinforce [(EasternAustralia, 5)] testGame == Nothing) `shouldBe` True
                (reinforce [(WesternAustralia, 3), (EasternAustralia, 2)] testGame == Nothing) `shouldBe` True

            it "Reinforcing a country owned by no-one" $ do
                (reinforce [(India, 5)] testGame == Nothing) `shouldBe` True
                (reinforce [(WesternAustralia, 3), (India, 2)] testGame == Nothing) `shouldBe` True

            it "Doesn't let reinforce with invalid number of troops" $ do
               (reinforce [(WesternAustralia,4)] testGame == Nothing) `shouldBe` True
               (reinforce [(WesternAustralia,6)] testGame == Nothing) `shouldBe` True

               (reinforce [(WesternAustralia, 5), (Siam, 2)] testGame == Nothing) `shouldBe` True
               (reinforce [(WesternAustralia, 2), (Siam, 2)] testGame == Nothing) `shouldBe` True

            -- Trying to reinforce with more than valid num of troops


    describe "Attack" $ do
        let testGame = (nextPhase) game
        context "Valid Inputs" $ do
            it "Correctly updates attackers,defenders and stdGen" $ do
                let t1g = (fromJust.attack WesternAustralia EasternAustralia TwoAtt TwoDef) testGame

                (troops t1g WesternAustralia == 2) `shouldBe` True
                (troops t1g EasternAustralia == 2) `shouldBe` True
                ((currentStdGen t1g) == mkStdGen 0) `shouldBe` False
                (phase t1g == Attack Normal) `shouldBe` True
            it "Correctly leaves game in WonBattle state if defenders wiped out" $ do
                let t1g = (fromJust.attack WesternAustralia EasternAustralia ThreeAtt OneDef .updateStdGen (mkStdGen 3).changeTroops WesternAustralia 1 .(changeTroops EasternAustralia (-2))) testGame
                (phase t1g == Attack (WonBattle WesternAustralia EasternAustralia ThreeAtt))

        context "Invalid Inputs" $ do
            it "Check it doesn't work in any other phase/miniphase" $ do
                ((attack WesternAustralia EasternAustralia OneAtt OneDef) game == Nothing) `shouldBe` True
                (((attack WesternAustralia EasternAustralia OneAtt OneDef).changeMiniPhase (WonBattle EasternAustralia WesternAustralia TwoAtt).nextPhase) game == Nothing) `shouldBe` True
                (((attack WesternAustralia EasternAustralia OneAtt OneDef).nextPhase.nextPhase) game == Nothing) `shouldBe` True

            it "Check it doesn't work if you try and attack with too many troops" $ do
                let t1g = changeTroops WesternAustralia (-2) testGame
                ((attack WesternAustralia EasternAustralia TwoAtt OneDef) game == Nothing) `shouldBe` True

            it "Doesn't let you attack with an invalid country pair" $ do
                ((attack WesternAustralia Peru OneAtt OneDef testGame == Nothing) `shouldBe` True)
                ((attack Siam Indonesia OneAtt OneDef testGame == Nothing) `shouldBe` True)
                ((attack EasternAustralia WesternAustralia OneAtt OneDef testGame == Nothing) `shouldBe` True)
                ((attack Peru Brazil OneAtt OneDef testGame == Nothing) `shouldBe` True)
                ((attack WesternAustralia WesternAustralia OneAtt OneDef testGame == Nothing) `shouldBe` True)
                ((attack EasternAustralia EasternAustralia OneAtt OneDef testGame == Nothing) `shouldBe` True)



    describe "Invade" $ do
        let testGame = nextPhase game
        context "Valid Inputs" $ do
            it "Check it moves the troops correctly" $ do
                let t1g = (fromJust.invade 2.changeMiniPhase (WonBattle WesternAustralia EasternAustralia TwoAtt).changeTroops EasternAustralia (-3)) testGame
                (owner t1g EasternAustralia == Red `shouldBe` True)
                (troops t1g EasternAustralia == 2 `shouldBe` True)
                (troops t1g WesternAustralia == 1 `shouldBe` True)

            it "Check it moves the troops correctly on another players turn" $ do
                let t1g = (fromJust.invade 2.changeMiniPhase (WonBattle EasternAustralia WesternAustralia TwoAtt).(changeTroops WesternAustralia (-3)) .nextPhase.nextTurn) testGame
                (owner t1g WesternAustralia == Blue `shouldBe` True)
                (troops t1g EasternAustralia == 1 `shouldBe` True)
                (troops t1g WesternAustralia == 2 `shouldBe` True)

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
