module RiskBoardSpec where

import Test.Hspec
import Test.QuickCheck
import Data.List
import RiskBoard
import qualified Data.MultiSet as M

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
      it "== works as expected for neighbours" $ do
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

  describe "isNeighbour" $ do
    context "Africa" $ do
      it "correctly identifies Madagascar and South Africa as neighbours" $ do
        isNeighbour Madagascar SouthAfrica `shouldBe` True
      it "correctly identifies Egypt and Congo as non-neighbours" $ do
        isNeighbour Egypt Congo `shouldBe` False
      it "correctly identifies Egypt and East Africa as neighbours" $ do
        isNeighbour Egypt EastAfrica `shouldBe` True
    context "Europe" $ do
      it "correctly identifies Northern Europe and Scandinavia as neighbours" $ do
        isNeighbour NorthernEurope Scandinavia `shouldBe` True
      it "correctly identifies Iceland and Ukraine as non-neighbours" $ do
        isNeighbour Iceland Ukraine `shouldBe` False
      it "correctly identifies Great Britain and Western Europe as neighbours" $ do
        isNeighbour GreatBritain WesternEurope `shouldBe` True
    context "Asia" $ do
      it "correctly identifies India and Siam as neighbours" $ do
        isNeighbour India Siam `shouldBe` True
      it "correctly identifies Middle East and Japan as non-neighbours" $ do
        isNeighbour MiddleEast Japan `shouldBe` False
      it "correctly identifies Ural and Siberia as neighbours" $ do
        isNeighbour Ural Siberia `shouldBe` True
    context "North America" $ do
      it "correctly identifies Eastern United States and Western United States as neighbours" $ do
        isNeighbour EasternUnitedStates WesternUnitedStates `shouldBe` True
      it "correctly identifies Quebec and Alaska as non-neighbours" $ do
        isNeighbour Quebec Alaska `shouldBe` False
      it "correctly identifies Ontario and Greenland as neighbours" $ do
        isNeighbour Ontario Greenland `shouldBe` True
    context "South America" $ do
      it "correctly identifies Brazil and Venezuela as neighbours" $ do
        isNeighbour Brazil Venezuela `shouldBe` True
      it "correctly identifies Argentina and Venezuela as non-neighbours" $ do
        isNeighbour Argentina Venezuela `shouldBe` False
      it "correctly identifies Peru and Argentina as neighbours" $ do
        isNeighbour Peru Argentina `shouldBe` True
    context "Australia" $ do
      it "correctly identifies Eastern Australia and Western Australia as neighbours" $ do
        isNeighbour EasternAustralia WesternAustralia `shouldBe` True
      it "correctly identifies Indonesia and Eastern Australia as non-neighbours" $ do
        isNeighbour Indonesia EasternAustralia `shouldBe` False
      it "correctly identifies New Guinea and Western Australia as neighbours" $ do
        isNeighbour NewGuinea WesternAustralia `shouldBe` True
    context "Between Continents" $ do
      it "correctly identifies Alaska and Kamchatka as neighbours" $ do
        isNeighbour Alaska Kamchatka `shouldBe` True
      it "correctly identifies Brazil and New Guinea as non-neighbours" $ do
        isNeighbour Brazil NewGuinea `shouldBe` False
      it "correctly identifies Afghanistan and Ukraine as neighbours" $ do
        isNeighbour Afghanistan Ukraine `shouldBe` True

  describe "neighbours" $ do
    context "Africa" $ do
      it "correctly identifies neighbours of Congo" $ do
        M.fromList (neighbours Congo) `shouldBe` M.fromList [EastAfrica, SouthAfrica, NorthAfrica]
    context "Europe" $ do
      it "correctly identifies neighbours of Great Britain" $ do
        M.fromList (neighbours GreatBritain)
          `shouldBe` M.fromList [Iceland, Scandinavia, NorthernEurope, WesternEurope]
    context "Asia" $ do
      it "correctly identifies neighbours of India" $ do
        M.fromList (neighbours India)
          `shouldBe` M.fromList [MiddleEast, Siam, Afghanistan, China]
    context "North America" $ do
      it "correctly identifies neighbours of Alberta" $ do
        M.fromList (neighbours Alberta)
          `shouldBe` M.fromList [NorthwestTerritory, Ontario, Alaska, WesternUnitedStates]
    context "South America" $ do
      it "correctly identifies neighbours of Peru" $ do
        M.fromList (neighbours Peru)
          `shouldBe` M.fromList [Venezuela, Brazil, Argentina]
    context "Australia" $ do
      it "correctly identifies neighbours of Indonisia" $ do
        M.fromList (neighbours Indonesia)
          `shouldBe` M.fromList [WesternAustralia, NewGuinea, Siam]
