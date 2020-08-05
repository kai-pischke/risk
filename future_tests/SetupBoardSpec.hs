module SetupBoardSpec where

import Test.Hspec
import SetupBoard
import RiskBoard
import State
import System.Random
import GameElements
import Data.Maybe

setupGame = emptyBoard [Red, Black]

justPlaceTroop c = fromJust.(placeTroop c)

addOneAllCountry:: SetupState -> SetupState
addOneAllCountry = cb 42

completeBoard:: Int -> [Country] -> SetupState -> SetupState
completeBoard 0 _ s = s
completeBoard n (c:cs) s = (completeBoard (n-1) (cs++[c]). justPlaceTroop c) s

cb:: Int -> SetupState -> SetupState
cb n s = completeBoard n [toEnum 0 ..] s

spec :: Spec
spec = do
    describe "placeTroop" $ do
        context "Invalid Inputs" $ do
            it "Returns Nothing if another person owns the country" $ do
                ((placeTroop Alaska .justPlaceTroop Alaska) setupGame == Nothing) `shouldBe` True
                ((placeTroop Alaska .justPlaceTroop Alaska. addOneAllCountry) setupGame == Nothing) `shouldBe` True

            it "Returns Nothing if incomplete and trying to add to a country already added to" $ do
                ((placeTroop Alaska .justPlaceTroop Siam .justPlaceTroop Alaska) setupGame == Nothing) `shouldBe` True

            it "Errors if try to add to a complete board" $ do
                ((print.placeTroop Alaska .cb 80) setupGame) `shouldThrow` anyErrorCall

    describe "completeBoardOwner" $ do
        context "Invalid Inputs" $ do
            it "Errors if try to apply to a incomplete or partially complete board" $ do
                ((print.completeBoardOwner (justPlaceTroop Alaska setupGame))  Alaska) `shouldThrow` anyErrorCall
                --((print.completeBoardOwner (addOneAllCountry setupGame)) Alaska) `shouldThrow` anyException

    describe "Creating a full board" $ do
        context "2 Player Board" $ do
            let completed2pBoard = cb 80 setupGame
            let b2p = newGame [Red, Black] (completeBoardOwner completed2pBoard) (mkStdGen 0)

            it "2 player game has correct owners" $ do
                (all (\x -> owner b2p (toEnum x) == Red) (filter even [0..41])) `shouldBe` True
                (all (\x -> owner b2p (toEnum x) == Black) (filter odd [0..41])) `shouldBe` True

            it "2 player game has correct number troops" $ do
                (all (\x -> troops b2p (toEnum x) == 2) [0..37]) `shouldBe` True
                (all (\x -> troops b2p (toEnum x) == 1) [38..41]) `shouldBe` True

        context "3 Player Game" $ do
            let completed4pBoard = cb 105 (emptyBoard [Red, Black, Blue])

            let b3p = newGame [Red, Black, Blue] (completeBoardOwner completed4pBoard) (mkStdGen 0)

            it "3 player game has correct owners" $ do
                (all (\x -> owner b3p (toEnum x) == Red) (filter (\x -> mod x 3 == 0) [0..41])) `shouldBe` True
                (all (\x -> owner b3p (toEnum x) == Black) (filter (\x -> mod x 3 == 1) [0..41])) `shouldBe` True
                (all (\x -> owner b3p (toEnum x) == Blue) (filter (\x -> mod x 3 == 2) [0..41])) `shouldBe` True

            it "3 player game has correct number troops" $ do
                (all (\x -> troops b3p (toEnum x) == 3) [0..20]) `shouldBe` True
                (all (\x -> troops b3p (toEnum x) == 2) [21..41]) `shouldBe` True
