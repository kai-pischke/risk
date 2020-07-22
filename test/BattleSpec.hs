module BattleSpec where

import Test.Hspec
import Test.QuickCheck
import Battle

spec :: Spec
spec = do
    describe "Defenders" $ do
        context "Eq" $ do 
            it "correctly deals with equal cases" $ do
                (OneDef == OneDef) `shouldBe` True
                (TwoDef == TwoDef) `shouldBe` True
            it "correctly deals with non-equal case" $ do
                (OneDef == TwoDef) `shouldBe` False
                (TwoDef == OneDef) `shouldBe` False
        context "Enum" $ do 
            it "works with fromEnum" $ do
                fromEnum OneDef `shouldBe` 1
                fromEnum TwoDef `shouldBe` 2
            it "works with toEnum" $ do
                (toEnum 1) `shouldBe` OneDef
                (toEnum 2) `shouldBe` TwoDef
    describe "Attackers" $ do
        context "Eq" $ do 
            it "correctly deals with equal cases" $ do
                (OneAtt == OneAtt) `shouldBe` True
                (TwoAtt == TwoAtt) `shouldBe` True
                (ThreeAtt == ThreeAtt) `shouldBe` True
            it "correctly deals with non-equal cases" $ do
                (OneAtt == TwoAtt) `shouldBe` False
                (OneAtt == ThreeAtt) `shouldBe` False
                (TwoAtt == OneAtt) `shouldBe` False
                (TwoAtt == ThreeAtt) `shouldBe` False
                (ThreeAtt == OneAtt) `shouldBe` False
                (ThreeAtt == TwoAtt) `shouldBe` False
        context "Enum" $ do 
            it "works with fromEnum" $ do
                fromEnum OneAtt `shouldBe` 1
                fromEnum TwoAtt `shouldBe` 2
                fromEnum ThreeAtt `shouldBe` 3
            it "works with toEnum" $ do
                (toEnum 1) `shouldBe` OneAtt
                (toEnum 2) `shouldBe` TwoAtt
                (toEnum 3) `shouldBe` ThreeAtt

