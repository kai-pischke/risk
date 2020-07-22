module StateSpec where

import Test.Hspec
import Test.QuickCheck
import State
import RiskBoard
import Battle
import System.Random

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
  describe "newgame" $ do

    context "Newly initialised game with players [Blue, Red, Green]" $ do
        it "correctly has no troops in Scandinavia" $ do
          (troops game Scandinavia == 0) `shouldBe` True
        it "correctly has no troops in Kamchatka" $ do
          (troops game Kamchatka == 0) `shouldBe` True
        it "correctly has no troops in North Africa" $ do
          (troops game NorthAfrica == 0) `shouldBe` True
        it "correctly has players in order Blue, Red, Green" $ do
          (turnOrder game == [Blue, Red, Green]) `shouldBe` True
        it "correctly has Reinforce as starting phase" $ do
          (phase game == Reinforce) `shouldBe` True
      where game = newGame [Blue, Red, Green] (mkStdGen 0)
