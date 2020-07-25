module StateSpec where

import Test.Hspec
import Test.QuickCheck
import State
import RiskBoard
import Battle
import System.Random

allRed :: GameState
allRed = newGame [Blue, Red, Green] (\x -> (Red, 0)) (mkStdGen 137)

spec :: Spec
spec = do
  describe "Player" $ do
    context "Eq" $ do
      it "correctly deals with equal cases" $ do
        (Blue == Blue) `shouldBe` True
        (Red == Red) `shouldBe` True
        (Black == Black) `shouldBe` True
      it "correctly deals with unequal cases" $ do
        (Blue == Black) `shouldBe` False
        (Red == Green) `shouldBe` False
        (Green == Yellow)  `shouldBe` False
  describe "MiniPhase" $ do
    context "Eq" $ do
      it "correctly deals with equal cases" $ do
        (Normal == Normal) `shouldBe` True
        (WonBattle GreatBritain WesternAustralia TwoAtt == WonBattle GreatBritain WesternAustralia TwoAtt) `shouldBe` True
      it "correctly deals with unequal cases" $ do
        (Normal == WonBattle GreatBritain Scandinavia TwoAtt) `shouldBe` False
        (WonBattle GreatBritain WesternAustralia TwoAtt == WonBattle GreatBritain WesternAustralia OneAtt) `shouldBe` False
        (WonBattle GreatBritain Scandinavia TwoAtt == WonBattle GreatBritain WesternAustralia TwoAtt) `shouldBe` False
        (WonBattle Scandinavia WesternAustralia TwoAtt == WonBattle GreatBritain WesternAustralia TwoAtt) `shouldBe` False
  describe "Phase" $ do
    context "Eq" $ do
      it "correctly deals with equal cases"  $ do
        (Reinforce == Reinforce) `shouldBe` True
        (Attack Normal == Attack Normal) `shouldBe` True
        (Attack (WonBattle GreatBritain WesternAustralia TwoAtt) == Attack (WonBattle GreatBritain WesternAustralia TwoAtt)) `shouldBe` True
      it "correctly deals with unequal cases" $ do
        (Reinforce == Fortify) `shouldBe` False
        (Attack Normal == Attack (WonBattle GreatBritain Scandinavia TwoAtt)) `shouldBe` False
        (Attack(WonBattle GreatBritain WesternAustralia TwoAtt) == Attack (WonBattle GreatBritain WesternAustralia OneAtt)) `shouldBe` False
        (Attack(WonBattle GreatBritain Scandinavia TwoAtt) == Attack (WonBattle GreatBritain WesternAustralia TwoAtt)) `shouldBe` False
        (Attack(WonBattle Scandinavia WesternAustralia TwoAtt) == Attack(WonBattle GreatBritain WesternAustralia TwoAtt)) `shouldBe` False
  describe "changeTroops" $ do
    let game = allRed
    let game' = changeTroops Brazil 3 game
    let game'' = changeTroops Brazil (-2) game'
    context  "Adding 3 troops to Brazil" $ do
      it "correctly increases the number of troops in Brazil" $ do
        (troops game' Brazil == troops game Brazil + 3) `shouldBe` True
      it "correctly doesn't increase the number of troops in Peru" $ do
        (troops game' Peru == troops game Peru) `shouldBe` True
      it "correctly doesn't increase the number of troops in Argentina" $ do
        (troops game' Argentina == troops game Argentina) `shouldBe` True
      it "correctly doesn't change the owner of Brazil" $ do
        (owner game' Brazil == owner game Brazil) `shouldBe` True
    context "Taking away 2 troops from Brazil " $ do
      it "correctly decreases the number of troops in Brazil" $ do
        (troops game'' Brazil == (troops game Brazil) + 1) `shouldBe` True
      it "correctly doesn't change the number of troops in Peru" $ do
        (troops game' Peru == troops game Peru) `shouldBe` True
      it "correctly doesn't change the number of troops in Argentina" $ do
        (troops game' Argentina == troops game Argentina) `shouldBe` True
      it "correctly doesn't change the owner of Brazil" $ do
        (owner game' Brazil == owner game Brazil) `shouldBe` True
  describe "changeOwner" $ do
    context "Changing owner of Madagascar" $ do
      let game = allRed
      let game' = changeOwner Madagascar Blue game
      let game'' = changeOwner Madagascar Green game'
      it "correctly changes owner to Blue" $ do
        (owner game' Madagascar == Blue) `shouldBe` True
      it "doesn't change owner of Egypt" $ do
        (owner game' Egypt == owner game Egypt) `shouldBe` True
      it "doesn't change owner of North Africa" $ do
        (owner game' NorthAfrica == owner game NorthAfrica) `shouldBe` True
      it "correctly changes owner again to Green" $ do
        (owner game'' Madagascar == Green) `shouldBe` True
  describe "nextTurn" $ do
    context "game with players [Blue, Red, Green]" $ do
      let game = allRed
      let game' = nextTurn game
      let game'' = nextTurn game'
      let game''' = nextTurn game''
      it "correctly updates first player to Red" $ do
        (turnOrder game' == [Red, Green, Blue]) `shouldBe` True
      it "correctly updates first player to Green" $ do
        (turnOrder game'' == [Green, Blue, Red]) `shouldBe` True
      it "correctly loops back round to Blue" $ do
        (turnOrder game''' == [Blue, Red, Green]) `shouldBe` True
      it "correctly resets phase to Reinforce from Attack" $ do
        let attGame = nextPhase game'
        (phase (nextTurn attGame) == Reinforce) `shouldBe` True
      it "correctly resets phase to Reinforce from Fortify" $ do
        let fortGame = nextPhase (nextPhase game')
        (phase (nextTurn fortGame) == Reinforce) `shouldBe` True
    context "game with players [Black]" $ do
      let game = newGame [Black] (\x -> (Blue, 0))(mkStdGen 0)
      let game' = nextTurn game
      let game'' = nextTurn game'
      it "correctly updates to Black once" $ do
        (turnOrder game' == [Black]) `shouldBe` True
      it "correctly updates to Black twice" $ do
        (turnOrder game'' == [Black]) `shouldBe` True
  describe "updateStdGen" $ do
    context "new game" $ do
      let game = allRed
      let game' = updateStdGen (mkStdGen 30) game
      it "correctly StdGen in game /= StdGen in game'" $ do
        (show (currentStdGen game) == show (currentStdGen game')) `shouldBe` False
  describe "nextPhase" $ do
    context "New Game" $ do
      let game = allRed
      let game' = nextPhase game
      let game'' = nextPhase game'
      let game''' = nextPhase game''
      it ("correctly starts on Reinforce") $ do
        (phase game == Reinforce) `shouldBe` True
      it ("correctly updates to Attack") $ do
        (phase game' == Attack Normal) `shouldBe` True
      it ("correctly updates to Fortify") $ do
        (phase game'' == Fortify) `shouldBe` True
      it ("correctly loops back to Reinforce") $ do
        (phase game''' == Reinforce) `shouldBe` True
      it ("correctly doesn't change turn order") $ do
        (turnOrder game'' == turnOrder game) `shouldBe` True
