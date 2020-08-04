module SetupBoardSpec where

import Test.Hspec
import SetupBoard
import RiskBoard
import State
import System.Random
import GameElements

setupGame = emptyBoard [Red, Black]

justPlaceTroop = fromJust.placeTroop

addOneAllCountry:: SetupState -> SetupState
addOneAllCountry s = foldr (\f x -> (fromJust.f) x) s fs
    where
        fs = map placeTroop [toEnum 0 ..]

completeBoard:: Int -> [Country] -> SetupState -> SetupState
completeBoard 0 _ s = s
completeBoard n (c:cs) s = (completeBoard (n-1) (cs++c). placeTroop c) s

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
                ((print.placeTroop Alaska .completeBoard 40) setupGame) `shouldThrow` anyException

    describe "completeBoardOwner" $ do
        context "Invalid Inputs" $ do
            it "Errors if try to apply to a incomplete or partially complete board" $ do
                ((print.completeBoardOwner) setupGame) `shouldThrow` anyException
                ((print.completeBoardOwner.addOneAllCountry) setupGame) `shouldThrow` anyException

    describe "Creating a full board" $ do
        context "2 Player Board" $ do
            let completed2pBoard = completeBoard 80 setupGame
            let b2p = newGame [Red, Black] (completeBoardOwner completedBoard) (mkStdGet 0)

            it "2 player game has correct owners" $ do
                (all (\x -> owner b (fromEnum x) == Red) (filter even [0..41])) `shouldBe` True
                (all (\x -> owner b (fromEnum x) == Black) (filter odd [0..41])) `shouldBe` True

            it "2 player game has correct number troops" $ do
                (all (\x -> troops b (fromEnum x) == 2) [0..37]) `shouldBe` True
                (all (\x -> troops b (fromEnum x) == 1) [38..41]) `shouldBe` True

        context "3 Player Game" $ do
            let completed4pBoard = completeBoard 105 (emptyBoard [Red, Black, Blue])

            let b3p = newGame [Red, Black, Blue] (completeBoardOwner completedBoard) (mkStdGet 0)

            it "3 player game has correct owners" $ do
                (all (\x -> owner b (fromEnum x) == Red) (filter (\x -> mod x 3 == 0) [0..41])) `shouldBe` True
                (all (\x -> owner b (fromEnum x) == Black) (filter (\x -> mod x 3 == 1) [0..41])) `shouldBe` True
                (all (\x -> owner b (fromEnum x) == Blue) (filter (\x -> mod x 3 == 2) [0..41])) `shouldBe` True

            it "3 player game has correct number troops" $ do
                (all (\x -> troops b (fromEnum x) == 3) [0..20]) `shouldBe` True
                (all (\x -> troops b (fromEnum x) == 2) [21..41]) `shouldBe` True
