module Moves (
    Country(..),
    reinforce,
    attack,
    invade,
    endAttack,
    fortify,
    skipFortify
) where


  ---- Imports --------------------------------
  import Battle
  import State
  import RiskBoard
  ---------------------------------------------

  ---- Helper Functions ------------------------
  currPlayer :: GameState -> Player
  currPlayer = head.turnOrder

  --change later to  calculate continent bonuses
  nReinforcements :: GameState -> Player -> Int
  nReinforcements _ _ = 5

  -----------------------------------------------

  ----Public Functions ---------------------

  reinforce :: [(Country, Int)] -> GameState -> Maybe GameState
  reinforce movs gs | phase gs == Reinforce && validMovList movs =
    Just $ nextPhase $ foldr ((.).(uncurry changeTroops)) id movs gs
                    | otherwise = Nothing
    where validMovList = valid (nReinforcements gs (currPlayer gs))
          valid 0 []  = True
          valid _ []  = False
          valid n ((c,t) : ms) = (owner gs c == currPlayer gs)
                                && (t > 0)
                                && valid (n-t) ms

  attack :: Country -> Country -> Attackers -> Defenders -> GameState -> Maybe GameState
  attack cAtt cDef att def gs | valid = (Just . makeChange (doBattle att def $ currentStdGen gs)) gs
                              | otherwise = Nothing
    where
      valid = (troops gs cAtt > fromEnum att) &&
              (troops gs cDef >= fromEnum def) &&
              (owner gs cAtt == currPlayer gs) &&
              (owner gs cDef /= currPlayer gs) &&
              (cDef `isNeighbour` cAtt) &&
              (phase gs == Attack Normal)
      makeChange (attLosses, defLosses, stdGen) =
        if (defLosses == troops gs cDef) --attacker has won a country
          then
           (changeMiniPhase (WonBattle cAtt cDef (toEnum nInvaders))) . regularChanges
          else regularChanges
        where regularChanges = (changeTroops cAtt (-attLosses)) . (changeTroops cDef (-defLosses)) . (updateStdGen stdGen)
              nInvaders = fromEnum att - attLosses

  -- Must invade with at least the number of attackers left, must leave at least 1 in the previous country
  invade :: Int -> GameState -> Maybe GameState
  invade nTroops gs = f (phase gs)
    where
      f (Attack (WonBattle cAtt cDef attLeft))
        | not (cAtt `isNeighbour` cDef) || (owner gs cAtt == owner gs cDef) || (owner gs cAtt /= currPlayer gs) = error "Impossible MiniPhase"
        | (nTroops >= fromEnum attLeft) && (nTroops < troops gs cAtt) =
          Just $ ((changeMiniPhase Normal) . (changeOwner cDef (currPlayer gs)). (changeTroops cAtt (-nTroops)) . (changeTroops cDef nTroops)) gs
        | otherwise = Nothing
      f _ = Nothing

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
