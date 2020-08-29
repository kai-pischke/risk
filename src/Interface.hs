{-|
Module      : Interface
Description : The functions for playing the game.
Maintainer  : Alex

This module wraps many of the functions in moves in additional validation logic and exposes a Hasskell version of the JSON
interface used to communicate with the Server.
-}
module Interface  (
  Game,
  Response(..),
  Request(..),
  empty,
  addPlayer,
  receive
) where

  ---- Imports -------------------------
  import Message as M
  import Moves
  import GameElements
  import SetupBoard
  import State
  import System.Random
  --------------------------------------

  ---- Types ---------------------------
  -- |The main type holding all the needed information about the game.
  data Game = GameWR [Player] StdGen
    | GameSetup SetupState [Player] StdGen
    | GamePlay GameState
    deriving (Show, Eq)
  --------------------------------------

  ---- Public Functions ----------------

  -- |Creates a new empty game with a given 'StdGen'.
  empty :: StdGen -> Game
  empty stdGen = (GameWR [] stdGen)

  --works if the waiting room isn't in order of players,
  --even though this should never happen in practice
  -- |Errors if we aren't in the waiting phase, or if the waiting room is full, or if the given waiting room is invalid (i.e. there are repeats)
  addPlayer :: Game -> (Player, Game)
  addPlayer (GameWR ps std)
    | repeats ps = error "Invalid waiting room: There are repeats"
    | length ps < 5 = (newPlayer, GameWR (ps ++ [newPlayer]) std)
    | otherwise = error "Waiting room is full"
    where newPlayer = head $ filter (flip notElem ps) players
  addPlayer _ = error "The game has started, we can't add new players"

  -- |Total function. For any 'Request', checks whether it is valid and returns an appropriate 'Response' message along with the new 'Game' with the correct changes made.
  receive :: Request -> Game -> (Response, Game)
  -- StartGame
  receive (Request s StartGame) g@(GameWR ps std)
    |length ps <= 1 = (Invalid NotEnoughPlayers s, g)
    |otherwise = let g' = GameSetup (emptyBoard ps) ps std in (General (toUpdate g'), g')
  receive (Request s StartGame) g = (Invalid NotInWaitingRoom s, g)

  -- PlaceTroop
  receive (Request s (PlaceTroop c)) g@(GameSetup sstate ps std) =
    if s == (head (setUpTurnOrder sstate))
      then
        case sstate of
          Complete _ -> (Invalid SetupComplete s, g)
          _ -> case placeTroop c sstate of
            Just (Complete boardState) -> let g' = newGame ps (completeBoardOwner (Complete boardState)) std in (General (Play g'), GamePlay g')
            Just sstate' -> (General (Setup sstate'), GameSetup sstate' ps std)
            Nothing -> (Invalid InvalidMove s, g)
      else (Invalid NotYourTurn s, g)

  receive (Request s (PlaceTroop _)) g = (Invalid NotInSetup s, g)


  -- Attack
  receive (Request s (M.Attack cAtt cDef att)) g@(GamePlay gstate)
    | s == currentPlayer gstate =
      case attack cAtt cDef att gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (Special NumDefenders defender, GamePlay gstate')
    | otherwise = (Invalid NotYourTurn s, g)
    where defender = owner gstate cDef
  receive (Request s (M.Attack _ _ _)) g = (Invalid NotInPlay s, g)

  -- Reinforce
  receive (Request s (M.Reinforce troopMap)) g@(GamePlay gstate)
    | s == currentPlayer gstate =
      case reinforce None troopMap gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (General (Play gstate'), GamePlay gstate')
    | otherwise = (Invalid NotYourTurn s, g)

  receive (Request s (M.Reinforce _)) g = (Invalid NotInPlay s, g)

  -- Fortify
  receive (Request s (M.Fortify cFrom cTo nTroops)) g@(GamePlay gstate)
    | s == currentPlayer gstate =
      case fortify cFrom cTo nTroops gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (General (Play gstate'), GamePlay gstate')
    | otherwise = (Invalid NotYourTurn s, g)

  receive (Request s (M.Fortify _ _ _)) g = (Invalid NotInPlay s, g)

  -- Invade
  receive (Request s (M.Invade nTroops)) g@(GamePlay gstate)
    | s == currentPlayer gstate =
      case invade nTroops gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> if (length (turnOrder gstate') > 1)
                          then (General(Play gstate'), GamePlay gstate')
                          else (GameWon (head $ turnOrder gstate'), GamePlay gstate')
    | otherwise = (Invalid NotYourTurn s, g)
  receive (Request s (Invade _)) g = (Invalid NotInPlay s, g)

  -- ChooseDefenders
  receive (Request s (ChooseDefenders def)) g@(GamePlay gstate)
    | valid =
      case chooseDefenders def gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (General (Play gstate'), GamePlay gstate')
    | otherwise = (Invalid NotRequestingDefenders s, g)
    where valid =
            case phase gstate of
              State.Attack (MidBattle _ cDef _) -> s == owner gstate cDef
              _ -> False

  receive (Request s (ChooseDefenders _)) g = (Invalid NotInPlay s, g)

  -- EndAttack
  receive (Request s EndAttack) g@(GamePlay gstate)
    | s == currentPlayer gstate =
      case endAttack gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (General (Play gstate'), GamePlay gstate')
    | otherwise = (Invalid NotYourTurn s, g)
  receive (Request s EndAttack) g = (Invalid InvalidMove s, g)

  -- SkipFortify
  receive (Request s SkipFortify) g@(GamePlay gstate)
    | s == currentPlayer gstate =
      case skipFortify gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (General (Play gstate'), GamePlay gstate')
    | otherwise = (Invalid NotYourTurn s, g)
  receive (Request s SkipFortify) g = (Invalid InvalidMove s, g)

  --------------------------------------

  ---- Private helper functions --------

  players :: [Player]
  players = [Black, Blue, Green, Red, Yellow]

  repeats :: Eq a => [a] -> Bool
  repeats [] = False
  repeats (x : xs) = (x `elem` xs ) || (repeats xs)

  toUpdate :: Game -> Update
  toUpdate (GameWR ps _) = WaitingRoom ps
  toUpdate (GameSetup state _ _) = Setup state
  toUpdate (GamePlay g) = Play g

  currentPlayer :: GameState -> Player
  currentPlayer = head . turnOrder
