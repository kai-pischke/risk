module RiskBoardSpec where

import Test.Hspec
import Test.QuickCheck
import Data.List
import RiskBoard

spec :: Spec
spec = do
  describe "Country" $ do
    context "Enum" $ do 
      it "correctly associates East Africa with the id 21" $ do
        fromEnum EastAfrica `shouldBe` 21
      it "correctly associates Greenland with the id 4" $ do
        fromEnum Greenland `shouldBe` 4
      it "correctly associates Ural with the id 36" $ do
        fromEnum Ural `shouldBe` 36
      it "correctly associates New Guinea with the id 40" $ do
        fromEnum NewGuinea `shouldBe` 40
      it "correctly associates Venezuela with the id 12" $ do
        fromEnum Venezuela `shouldBe` 12
    context "Eq" $ do 
      it "== works as expected for Iceland" $ do
        (Iceland == Iceland) `shouldBe` True
      it "== works as expected for different countries" $ do
        (NorthwestTerritory == Kamchatka) `shouldBe` False
      it "== works as expected for neighbors" $ do
        (Mongolia == Irkutsk) `shouldBe` False
    context "Ord" $ do 
      it "< works as expected (true case)" $ do
        (China < Ural) `shouldBe` True
      it "< works as expected (false case)" $ do
        (Irkutsk < NorthAfrica) `shouldBe` False
    context "Show" $ do 
      it "works for Scandinavia" $ do
        show Scandinavia `shouldBe` "Scandinavia"
      it "works for Western Europe" $ do
        show WesternEurope `shouldBe` "Western Europe"
      it "works for Eastern Australia" $ do
        show EasternAustralia `shouldBe` "Eastern Australia"
      it "works for Western United States" $ do
        show WesternUnitedStates `shouldBe` "Western United States"

  describe "isNeighbor" $ do
    context "Africa" $ do 
      it "correctly identifies Madagascar and South Africa as neighbors" $ do
        isNeighbor Madagascar SouthAfrica `shouldBe` True
      it "correctly identifies Egypt and Congo as non-neighbors" $ do
        isNeighbor Egypt Congo `shouldBe` False
      it "correctly identifies Egypt and East Africa as neighbors" $ do
        isNeighbor Egypt EastAfrica `shouldBe` True
    context "Europe" $ do 
      it "correctly identifies Northern Europe and Scandinavia as neighbors" $ do
        isNeighbor NorthernEurope Scandinavia `shouldBe` True
      it "correctly identifies Iceland and Ukraine as non-neighbors" $ do
        isNeighbor Iceland Ukraine `shouldBe` False
      it "correctly identifies Great Britain and Western Europe as neighbors" $ do
        isNeighbor GreatBritain WesternEurope `shouldBe` True
    context "Asia" $ do 
      it "correctly identifies India and Siam as neighbors" $ do
        isNeighbor India Siam `shouldBe` True
      it "correctly identifies Middle East and Japan as non-neighbors" $ do
        isNeighbor MiddleEast Japan `shouldBe` False
      it "correctly identifies Ural and Siberia as neighbors" $ do
        isNeighbor Ural Siberia `shouldBe` True
    context "North America" $ do
      it "correctly identifies Eastern United States and Western United States as neighbors" $ do
        isNeighbor EasternUnitedStates WesternUnitedStates `shouldBe` True
      it "correctly identifies Quebec and Alaska as non-neighbors" $ do
        isNeighbor Quebec Alaska `shouldBe` False
      it "correctly identifies Ontario and Greenland as neighbors" $ do
        isNeighbor Ontario Greenland `shouldBe` True
    context "South America" $ do
      it "correctly identifies Brazil and Venezuela as neighbors" $ do
        isNeighbor Brazil Venezuela `shouldBe` True
      it "correctly identifies Argentina and Venezuela as non-neighbors" $ do
        isNeighbor Argentina Venezuela `shouldBe` False
      it "correctly identifies Peru and Argentina as neighbors" $ do
        isNeighbor Peru Argentina `shouldBe` True
    context "Australia" $ do
      it "correctly identifies Eastern Australia and Western Australia as neighbors" $ do
        isNeighbor EasternAustralia WesternAustralia `shouldBe` True
      it "correctly identifies Indonesia and Eastern Australia as non-neighbors" $ do
        isNeighbor Indonesia EasternAustralia `shouldBe` False
      it "correctly identifies New Guinea and Western Australia as neighbors" $ do
        isNeighbor NewGuinea WesternAustralia `shouldBe` True
    context "Between Continents" $ do
      it "correctly identifies Alaska and Kamchatka as neighbors" $ do
        isNeighbor Alaska Kamchatka `shouldBe` True
      it "correctly identifies Brazil and New Guinea as non-neighbors" $ do
        isNeighbor Brazil NewGuinea `shouldBe` False
      it "correctly identifies Afghanistan and Ukraine as neighbors" $ do
        isNeighbor Afghanistan Ukraine `shouldBe` True


