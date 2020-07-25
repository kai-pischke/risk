module BattleSpec where

import Test.Hspec
import Test.QuickCheck
import System.Random
import Battle

instance Arbitrary Attackers where
    arbitrary = oneof $ map return [OneAtt, TwoAtt, ThreeAtt]
    
instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary 
        
instance Arbitrary Defenders where
    arbitrary = oneof $ map return [OneDef, TwoDef]

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
    describe "doBattle" $ do
        it "ensures total losses are correct" $ property $
               \a d g -> let (x,y,_) = doBattle a d g
                         in x+y == min (fromEnum a) (fromEnum d)
        it "ensures 0 <= attackers lost <= attackers" $ property $
               \a d g -> let (x,_,_) = doBattle a d g
                         in 0 <= x && x <= (fromEnum a)
        it "ensures 0 <= defenders lost >= defenders" $ property $
               \a d g -> let (_,y,_) = doBattle a d g
                         in 0 <= y && y <= (fromEnum d)


