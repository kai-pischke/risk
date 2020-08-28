{-|
Module      : Moves
Description : Core game logic.
Maintainer  : Alex

The basic implementation of the rules for the game, verifying individual actions that manipulate the state.
-}
module Moves (
    Country(..),
    reinforce,
    attack,
    chooseDefenders,
    invade,
    endAttack,
    fortify,
    skipFortify
) where


  ---- Imports --------------------------------
  import Battle
  import State
  import RiskBoard
  import GameElements

  import System.Random
  import Data.Array.ST
  import Control.Monad
  import Control.Monad.ST
  import Data.STRef
  ---------------------------------------------

  ---- Helper Functions ------------------------
  currPlayer :: GameState -> Player
  currPlayer = head.turnOrder

  --change later to  calculate continent bonuses
  nReinforcements :: GameState -> TradeIn -> Player -> Int
  nReinforcements g t p = tradeInBonus t + territoryBonus g p + continentBonus g p

  tradeInBonus :: TradeIn -> Int
  tradeInBonus None = 0
  tradeInBonus (OneSet s) = getValue s
  tradeInBonus (TwoSet s1 s2) = getValue s1 + getValue s2

  getValue (Infantry, Infantry, Infantry) = 4
  getValue (Cavalry, Cavalry, Cavalry) = 6
  getValue (Artillery, Artillery, Artillery) = 8
  getValue (Wild, x, y) = getValue (Infantry, x, y)
                          `max` getValue (Cavalry, x, y)
                          `max` getValue (Artillery, x, y)

  getValue (x, Wild, y) = getValue (Wild, x, y)
  getValue (x, y, Wild) = getValue (Wild, x, y)
  getValue (x, y, z) = if (x /= y && y /= z && x /= z)
                         then 10
                         else 0

  territoryBonus :: GameState -> Player -> Int
  territoryBonus g p = max (territoriesOwned `div` 3) 3
    where territoriesOwned = length (filter ((== p).owner g) [toEnum 0 :: Country ..])

  continentBonus :: GameState -> Player -> Int
  continentBonus g p = australiaBonus g p
                     + africaBonus g p
                     + southAmericaBonus g p
                     + europeBonus g p
                     + asiaBonus g p
                     + northAmericaBonus g p

  australiaBonus :: GameState -> Player -> Int
  australiaBonus g p | all ((== p).owner g) australia = 2
                     | otherwise = 0
  africaBonus :: GameState -> Player -> Int
  africaBonus g p | all ((== p).owner g) africa = 3
                  | otherwise = 0

  southAmericaBonus :: GameState -> Player -> Int
  southAmericaBonus g p | all ((== p).owner g) southAmerica = 2
                        | otherwise = 0

  asiaBonus :: GameState -> Player -> Int
  asiaBonus g p | all ((== p).owner g) asia = 7
                      | otherwise = 0

  europeBonus :: GameState -> Player -> Int
  europeBonus g p | all ((== p).owner g) europe = 5
                | otherwise = 0

  northAmericaBonus :: GameState -> Player -> Int
  northAmericaBonus g p | all ((== p).owner g) northAmerica = 5
                        | otherwise = 0

  -----------------------------------------------

  ----Public Functions ---------------------

  reinforce :: TradeIn -> [(Country, Int)] -> GameState -> Maybe GameState
  reinforce trade movs gs | validTrade trade && phase gs == Reinforce && validMovList movs =
    Just $ nextPhase $ (useCards trade) $ foldr ((.).(uncurry changeTroops)) id movs gs
                    | otherwise = Nothing
    where validMovList = valid (nReinforcements gs trade (currPlayer gs))
          valid 0 []  = True
          valid _ []  = False
          valid n ((c,t) : ms) = (owner gs c == currPlayer gs)
                                && (t > 0)
                                && valid (n-t) ms
          validTrade None = True
          validTrade (OneSet s) = t s
          validTrade (TwoSet s1 s2) = t s1 && t s2
          t (Wild, _, _) = True
          t (_, Wild, _) = True
          t (_, _, Wild) = True
          t (x, y, z) = (x == y && y == z) || (x /= y && x /= z && y /= z)
          useCards None = id
          useCards (OneSet (x, y, z)) = (useCard (currPlayer gs) x) . (useCard (currPlayer gs) y) . (useCard (currPlayer gs) z)
          useCards (TwoSet s1 s2) = (useCards (OneSet s1)) . (useCards (OneSet s2))

  attack :: Country -> Country -> Attackers -> GameState -> Maybe GameState
  attack cAtt cDef att gs | valid = (Just . changeMiniPhase (MidBattle cAtt cDef att)) gs
                          | otherwise = Nothing
    where
      valid = (troops gs cAtt > fromEnum att) &&
              (owner gs cAtt == currPlayer gs) &&
              (owner gs cDef /= currPlayer gs) &&
              (cDef `isNeighbour` cAtt) &&
              (phase gs == Attack Normal)

  chooseDefenders :: Defenders -> GameState -> Maybe GameState
  chooseDefenders def gs = f (phase gs)
    where f (Attack (MidBattle cAtt cDef att)) | troops gs cDef >= fromEnum def = (Just . makeChange (doBattle att def $ currentStdGen gs)) gs
                                               | otherwise = Nothing
            where makeChange (attLosses, defLosses, stdGen) =
                    if (defLosses == troops gs cDef) --attacker has won a country
                      then
                      (changeMiniPhase (WonBattle cAtt cDef (toEnum nInvaders))) . regularChanges
                      else regularChanges
                    where regularChanges = (changeTroops cAtt (-attLosses)) . (changeTroops cDef (-defLosses)) . (updateStdGen stdGen)
                          nInvaders = fromEnum att - attLosses
          f _ = Nothing


  -- Must invade with at least the number of attackers left, must leave at least 1 in the previous country
  invade :: Int -> GameState -> Maybe GameState
  invade nTroops gs = f (phase gs)
    where
      f (Attack (WonBattle cAtt cDef attLeft))
        | not (cAtt `isNeighbour` cDef) || (owner gs cAtt == owner gs cDef) || (owner gs cAtt /= currPlayer gs) = error "Impossible MiniPhase"
        | (nTroops >= fromEnum attLeft) && (nTroops < troops gs cAtt) =
          Just $ ((changeMiniPhase Normal) .forceDiscard . (tryKick (owner gs cAtt) (owner gs cDef)) . (drawCard (currPlayer gs)) . (changeOwner cDef (currPlayer gs)). (changeTroops cAtt (-nTroops)) . (changeTroops cDef nTroops)) gs
        | otherwise = Nothing
        where forceDiscard gs' = if (length (cards gs' (currPlayer gs')) >= 6)
                                   then changeMiniPhase TimeToTrade gs'
                                   else gs'
      f _ = Nothing
      tryKick :: Player -> Player -> GameState -> GameState
      tryKick pAtt pDef = if all ((/= pDef).owner gs) [toEnum 0 :: Country ..]
                        then kick pAtt pDef
                        else id

  trade :: TradeIn -> [(Country, Int)] -> GameState -> Maybe GameState
  trade tr movs gs | validTrade tr && phase gs == Attack TimeToTrade && validMovList movs =
    Just $ nextPhase $ (useCards tr) $ foldr ((.).(uncurry changeTroops)) id movs gs
                    | otherwise = Nothing
    where validMovList = valid (tradeInBonus tr)
          valid 0 []  = True
          valid _ []  = False
          valid n ((c,t) : ms) = (owner gs c == currPlayer gs)
                                && (t > 0)
                                && valid (n-t) ms
          validTrade None = True
          validTrade (OneSet s) = t s && length (cards gs (currPlayer gs)) - 3 < 5
          validTrade (TwoSet s1 s2) = t s1 && t s2 && length (cards gs (currPlayer gs)) - 3 > 4 && length (cards gs (currPlayer gs)) - 6 < 5
          t (Wild, _, _) = True
          t (_, Wild, _) = True
          t (_, _, Wild) = True
          t (x, y, z) = (x == y && y == z) || (x /= y && x /= z && y /= z)
          useCards None = id
          useCards (OneSet (x, y, z)) = (useCard (currPlayer gs) x) . (useCard (currPlayer gs) y) . (useCard (currPlayer gs) z)
          useCards (TwoSet s1 s2) = (useCards (OneSet s1)) . (useCards (OneSet s2))

  endAttack :: GameState -> Maybe GameState
  endAttack gs = f (phase gs)
    where f (Attack Normal) = Just (nextPhase gs)
          f _ = Nothing

  fortify :: Country -> Country -> Int -> GameState -> Maybe GameState
  fortify cFrom cTo nTroops gs
    | valid = (Just . nextTurn . (changeTroops cFrom (-nTroops)) . (changeTroops cTo nTroops)) gs
    | otherwise = Nothing
    where valid = (phase gs == Fortify) &&
                  (nTroops < troops gs cFrom) &&
                  (nTroops > 0) &&
                  (owner gs cFrom == currPlayer gs) &&
                  (owner gs cTo == currPlayer gs) &&
                  (cTo `isNeighbour` cFrom)

  skipFortify :: GameState -> Maybe GameState
  skipFortify gs | phase gs == Fortify = Just (nextTurn gs)
                 | otherwise = Nothing

-------------------------------------------------------------------------------------------------------
