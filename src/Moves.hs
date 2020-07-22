module Moves (
    reinforce,
    attack,
    invade,
    endAttack,
    fortify,
    skipFortify
) where

  import Battle
  import State
  
  reinforce :: [(Country, Int)] -> GameState -> Maybe GameState
  reinforce = undefined

  attack :: Attackers -> Defenders -> Country -> Country -> GameState -> Maybe GameState
  attack = undefined

  invade :: Int -> GameState -> Maybe GameState
  invade = undefined

  endAttack :: GameState -> Maybe GameState
  endAttack = undefined

  fortify :: Country -> Country -> Int -> GameState -> Maybe GameState
  fortify = undefined

  skipFortify :: GameState -> Maybe GameState
  skipFortify = undefined
