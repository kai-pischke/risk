module SetupSpec where

import Test.Hspec
import SetupBoard
import RiskBoard

setupGame = emptyBoard [Red, Black]

justPlaceTroop = fromJust.placeTroop

addOneAllCountry:: SetupState -> SetupState
addOneAllCountry s = foldr (\f x -> (fromJust.f) x) s fs
    where
        fs = map placeTroop [toEnum 0 ..]

completeBoard:: SetupState -> SetupState
completeBoard = undefined

spec :: Spec
spec = do
    describe "placeTroop" $ do
        context "Invalid Inputs" $ do
            it "Returns Nothing if another person owns the country" $ do
                ((placeTroop Alaska .justPlaceTroop Alaska) setupGame == Nothing) `shouldBe` True
                ((placeTroop Alaska .justPlaceTroop Alaska .addOneAllCountry)setupGame == Nothing) `shouldBe` True

            it "Returns Nothing if incomplete and trying to add to a country already added to" $ do
                ((placeTroop Alaska .justPlaceTroop Siam .justPlaceTroop Alaska) setupGame == Nothing) `shouldBe` True

            it "Errors if try to add to a complete board" $ do
                ((placeTroop Alaska .completeBoard) setupGame) `shouldThrow` anyException

    describe "completeBoardOwner" $ do
        context "Invalid Inputs" $ do
            it "Errors if try to apply to a incomplete or partially complete board" $ do
                ((completeBoardOwner) setupGame) `shouldThrow` anyException
                ((completeBoardOwner. addOneAllCountry) setupGame) `shouldThrow` anyException

    describe "Creating a full board" $ do
