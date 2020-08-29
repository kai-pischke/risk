module StateSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import State
import RiskBoard
import Battle
import System.Random
import GameElements

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
  describe "newGame" $ do
    context "All countries owned by Red, players are [Blue, Red, Green]" $ do
      it "correctly reinforce is first phase" $ do
        (phase allRed) `shouldBe` Reinforce
      it "correctly has turnorder [Blue, Red, Green]" $ do
        (turnOrder allRed) `shouldBe` [Blue, Red, Green]
      it "correctly has owner of Madagascar as Red" $ do
        (owner allRed Madagascar) `shouldBe` Red
      it "correctly has owner of Ukraine as Red" $ do
        (owner allRed Ukraine) `shouldBe` Red
      it "correctly no troops in Madagacar" $ do
        (troops allRed Madagascar) `shouldBe` 0
    context "Initialised with countries owned by a player not in the player list, different numbers of troops" $ do
      let game = newGame [Yellow, Black, Blue] (\x -> (Green, fromEnum x)) (mkStdGen 45)
      it "correctly has Madagascar owned by Green" $ do
        (owner game Madagascar) `shouldBe` Green
      it "correctly owner of North Africa is Green" $ do
        (owner game NorthAfrica) `shouldBe` Green
      it "has correct number of troops in Madagascar" $ do
        (troops game Madagascar) `shouldBe` (fromEnum Madagascar)
      it "has correct number of troops in North Africa" $ do
        (troops game NorthAfrica) `shouldBe` (fromEnum NorthAfrica)
      it "correctly doesn't alter turnOrder to include Green" $ do
        (turnOrder game) `shouldBe` [Yellow, Black, Blue]
    context "Repeat countries in player list" $ do
      let game = newGame [Yellow, Black, Yellow, Blue] (\x -> (Green, fromEnum x)) (mkStdGen 45)
      it "correctly allows for repeats in turnOrder" $ do
        (turnOrder game) `shouldBe` [Yellow, Black, Yellow, Blue]
    context "Empty player list" $ do
      it "correctly throws an error when given empty player list" $ do
        evaluate (newGame [] (\x -> (Green, 0)) (mkStdGen 45)) `shouldThrow` anyException

  describe "changeTroops" $ do
    let game = allRed
    let game' = changeTroops Brazil 3 game
    let game'' = changeTroops Brazil (-2) game'
    let game''' = changeTroops Brazil (-2) game''
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
        (troops game'' Peru == troops game Peru) `shouldBe` True
      it "correctly doesn't change the number of troops in Argentina" $ do
        (troops game'' Argentina == troops game Argentina) `shouldBe` True
      it "correctly doesn't change the owner of Brazil" $ do
        (owner game'' Brazil == owner game Brazil) `shouldBe` True
    context "Taking another 2 away from Brazil" $ do
      it "correctly allows the number of troops in Brazil to be negative" $ do
        (troops game''' Brazil) `shouldBe` (-1)
  describe "changeOwner" $ do
    context "Changing owner of Madagascar" $ do
      let game = newGame [Red, Green, Blue] (\x -> (Red, fromEnum x)) (mkStdGen 13)
      let game' = changeOwner Madagascar Blue game
      let game'' = changeOwner Madagascar Green game'
      let game''' = changeOwner Madagascar Black game''
      it "correctly changes owner to Blue" $ do
        (owner game' Madagascar == Blue) `shouldBe` True
      it "correctly doesn't change number of troops" $ do
        (troops game Madagascar == troops game' Madagascar) `shouldBe` True
      it "doesn't change owner of Egypt" $ do
        (owner game' Egypt == owner game Egypt) `shouldBe` True
      it "doesn't change owner of North Africa" $ do
        (owner game' NorthAfrica == owner game NorthAfrica) `shouldBe` True
      it "correctly changes owner again to Green" $ do
        (owner game'' Madagascar == Green) `shouldBe` True
      it "correctly doesn't change number of troops in Madagascar" $  do
        (troops game Madagascar == troops game' Madagascar) `shouldBe` True
      it "correctly allows owner to  be changes to a player not in turnOrder" $ do
        (owner game''' Madagascar) `shouldBe` Black
      it "correctly doesn't alter turnOrder" $ do
        (turnOrder game''') `shouldBe` [Red, Green, Blue]
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
    context "game with repeats in turnOrder, [Black, Yellow, Black, Blue]" $ do
      let game = newGame [Black, Yellow, Black, Blue] (\x -> (Black, fromEnum x)) (mkStdGen 546)
      let game' = nextTurn game
      let game'' = nextTurn game'
      let game''' = nextTurn game''
      let game'''' = nextTurn game'''
      it "correctly updates to Yellow" $ do
        (turnOrder game') `shouldBe` [Yellow, Black, Blue, Black]
      it "correctly updates to Black again" $ do
        (turnOrder game'') `shouldBe` [Black, Blue, Black, Yellow]
      it "correctly updates to Blue" $ do
        (turnOrder game''') `shouldBe` [Blue, Black, Yellow, Black ]
      it "correctly loops back to Black" $ do
        (turnOrder game'''') `shouldBe` [Black, Yellow, Black, Blue]
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
  describe "changeMiniPhase" $ do
    let game = allRed
    let reinforceChange = changeMiniPhase (WonBattle Peru Argentina TwoAtt) game
    let game' = nextPhase game
    let changeToWonBattle = changeMiniPhase (WonBattle Peru Argentina TwoAtt) game'
    let changeToWonBattle2 = changeMiniPhase (WonBattle India Brazil TwoAtt) game'
    let changeToNormal = changeMiniPhase Normal changeToWonBattle
    let nextFromNormal = nextPhase changeToNormal
    let nextFromWonBattle = nextPhase changeToWonBattle
    let fortified = changeMiniPhase (WonBattle India Brazil OneAtt) nextFromNormal
    context "New Game" $ do
      it "correctly does nothing when in Reinforce phase" $ do
        reinforceChange `shouldBe` game
    context "In normal attack phase" $ do
      it "correctly changes to WonBattle for adjacent countries"  $ do
        (phase changeToWonBattle) `shouldBe` (Attack (WonBattle Peru Argentina TwoAtt))
      it "correctly changes to WonBattle for non-adjacent countries" $ do
        (phase changeToWonBattle2) `shouldBe` (Attack (WonBattle India Brazil TwoAtt))
      it "correctly sets hasDrawn to True" $ do
        (hasDrawn changeToWonBattle) `shouldBe` True
        (hasDrawn changeToWonBattle) `shouldBe` True
    context "In WonBattle attack phase" $ do
      it "correctly changes to Normal phase" $ do
        phase changeToNormal `shouldBe` (Attack Normal)
    context "calling nextPhase" $ do
      it "correctly goes to Fortify when in normal attack phase" $ do
        (phase nextFromNormal) `shouldBe` Fortify
      it "correctly goes to Fortify when in WonBattle phase" $ do
        (phase nextFromWonBattle) `shouldBe` Fortify
      it "correctly no difference in state whether phase changes from Normal or WonBattle MiniPhase" $ do
        nextFromWonBattle `shouldBe` nextFromNormal
    context "In Fortify phase" $ do
      it "correctly no change when changeMiniPhase called" $ do
        fortified `shouldBe` nextFromNormal

  describe "drawCard" $ do
    let game = allRed
    let draw1 = drawCard Red game
    let draw2 = drawCard Red draw1
    context "Valid draws" $ do
      it "Correctly adds card into player's hand" $ do
        length (cards draw1 Red) `shouldBe` 1
        length (cards draw2 Red) `shouldBe` 2
      it "Correctly doesn't change other players' hands" $ do
        cards draw1 Blue `shouldBe` []
        cards draw1 Green `shouldBe` []
    context "Invalid Draws" $ do
      it "Correctly errors when a player not in the game draws a card" $ do
        print (drawCard Black game) `shouldThrow` anyException

  describe "useCard" $ do
    let game = allRed
    let draw1 = drawCard Red game
    let draw2 = drawCard Red draw1
    let draw3 = drawCard Red draw2
    let drawAll = (foldr (.) id (replicate 42 (drawCard Red))) game
    let game' = useCard Red (head (cards drawAll Red)) drawAll
    let cardNotDrawn = head (filter (`notElem` (cards draw3 Red)) [Artillery, Infantry, Cavalry, Wild])
    context "Valid uses" $ do
      it "correctly removes card from hand" $ do
        cards (useCard Red (head (cards draw3 Red)) draw3) Red `shouldBe` tail (cards draw3 Red)
      it "correctly reshuffles discard pile when cards run out" $ do
        cards (drawCard Red game') Red `shouldBe` cards drawAll Red
    context "Invalid uses" $ do
      it "correctly errors when the player doesn't have the card" $ do
        print (useCard Blue Artillery draw3) `shouldThrow` anyException
        print (useCard Red cardNotDrawn draw3) `shouldThrow` anyException
      it "correctly errors when a player not in the game uses a card" $ do
        print (drawCard Black game) `shouldThrow` anyException

  describe "kick" $ do
    let game = allRed
    let g' = (drawCard Blue . drawCard Blue . drawCard Blue) game
    let g'' = kick Red Blue g'
    context "Valid kicks" $ do
      it "correctly removes kicked player" $ do
        turnOrder g'' `shouldSatisfy` notElem Blue
      it "correctly gives cards to the attacker" $ do
        cards g'' Red `shouldBe` cards  g' Blue
    context "Invalid kicks" $ do
      it "correctly errors when the attacker not in turnOrder" $ do
        print (kick Black Red game) `shouldThrow` anyException
      it "correctly errors when the defender isn't in turnOrder" $ do
        print (kick Red Yellow game) `shouldThrow` anyException
