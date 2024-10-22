{-|
Module      : Message
Description : Haskell requests and responses.
Maintainer  : Alex

This module defines the messages we make use of in 'Interface'.
-}
module Message (
  Response (..),
  Update (..),
  Question (..),
  Error (..),
  Request (..),
  RequestType (..),
) where

  ---- Imports -------------------
  import GameElements
  import SetupBoard
  import RiskBoard
  import State
  import Battle
  --------------------------------

  ---- Types ---------------------

  data Response = General Update | Special Question Player | Invalid Error Player | GameWon Player
    deriving (Eq, Show)

  data Update = WaitingRoom [Player] | Setup SetupState | Play GameState
    deriving (Show)

  instance Eq Update where
    WaitingRoom xs == WaitingRoom ys = xs == ys
    Setup s1 == Setup s2 = s1 == s2
    Play gs1 == Play gs2 =
      phase gs1 == phase gs2
      && turnOrder gs1 == turnOrder gs2
      && map (owner gs1) countries == map (owner gs2) countries
      && map (troops gs1) countries == map (troops gs2) countries
      where countries = [minBound :: Country .. maxBound :: Country]
    _ == _ = False

  data Question = NumDefenders | GetTrade
    deriving (Eq, Show)

  data Error = InvalidMove | NotYourTurn | NotEnoughPlayers | NotInWaitingRoom | SetupComplete | NotInSetup | NotInPlay | NotRequestingDefenders
    deriving (Eq, Show)

  data Request = Request Player RequestType
    deriving (Eq, Show)

  data RequestType =
    StartGame
    | PlaceTroop Country
    | Attack Country Country Attackers
    | Reinforce TradeIn [(Country, Int)]
    | Fortify Country Country Int
    | Invade Int
    | ChooseDefenders Defenders
    | EndAttack
    | SkipFortify
    | Trade TradeIn [(Country, Int)]
    | SaveGame
    | LoadGame Int
    deriving (Eq, Show)
  -----------------------------------
