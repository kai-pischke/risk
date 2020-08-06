module Interface  (
  Game,
  Response,
  Request,
  empty,
  addPlayer,
  receive
) where

  ---- Imports -------------------------
  import Message
  import Moves
  import GameElements
  --------------------------------------

  ---- Types ---------------------------
  type Game = Update
  --------------------------------------

  ---- Public Functions ----------------

  empty :: Game
  empty = undefined

  addPlayer :: Game -> (Game, Player)
  addPlayer = undefined

  receive :: Request -> Game -> (Response, Game)
  receive = undefined

  --------------------------------------
