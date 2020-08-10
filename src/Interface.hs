module Interface  (
  Game,
  Response,
  Request,
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
  import Battle
  import RiskBoard
  import Data.Maybe --temp
  --------------------------------------

  ---- Types ---------------------------
  data Game = GameWR [Player] StdGen
    | GameSetup SetupState [Player] StdGen
    | GamePlay GameState
    | MiniState Country Country Attackers GameState
    deriving (Show, Eq)
  --------------------------------------

  ---- Public Functions ----------------

  empty :: StdGen -> Game
  empty stdGen = (GameWR [] stdGen)

  --works if the waiting room isn't in order of players,
  --even though this should never happen in practice
  addPlayer :: Game -> (Game, Player)
  addPlayer (GameWR ps std)
    | repeats ps = error "Invalid waiting room: There are repeats"
    | length ps < 5 = (GameWR (ps ++ [newPlayer]) std, newPlayer)
    | otherwise = error "Waiting room is full"
    where newPlayer = head $ filter (flip notElem ps) players
  addPlayer _ = error "The game has started, we can't add new players"

  receive :: Request -> Game -> (Response, Game)
  -- StartGame
  receive (Request s StartGame) g@(GameWR ps std)
    |length ps <= 1 = (Invalid NotEnoughPlayers s, g)
    |otherwise = let g' = GameSetup (emptyBoard ps) ps std in (General (toUpdate g'), g')
  receive (Request s StartGame) g = (Invalid NotInWaitingRoom s, g)

  -- PlaceTroop
  receive (Request s (PlaceTroop c)) g@(GameSetup sstate ps std) =
    case sstate of
      Complete boardState -> (Invalid SetupComplete s, g)
      _ -> case placeTroop c sstate of
        Just (Complete boardState) -> let g' = newGame ps (completeBoardOwner (Complete boardState)) std in (General (Play g'), GamePlay g')
        Just sstate' -> (General (Setup sstate'), GameSetup sstate' ps std)
        Nothing -> (Invalid InvalidMove s, g)

  receive (Request s (PlaceTroop _)) g = (Invalid NotInSetup s, g)


  -- Attack
  receive (Request s (M.Attack cAtt cDef att)) g@(GamePlay gstate)
    | validMove = let g' = MiniState cAtt cDef att gstate in
      (Special NumDefenders defender, g')
    | otherwise = (Invalid InvalidMove s, g)
    where attacker = owner gstate cAtt
          defender = owner gstate cDef
          --Have to add in checks here instead of using the ones in moves otherwise all errors will be sent to person choosing defenders
          validMove =
            (s == currentPlayer gstate) &&
            (troops gstate cAtt > fromEnum att) &&
            (attacker == s) &&
            (defender /= s) &&
            (cDef `isNeighbour` cAtt) &&
            (phase gstate == State.Attack Normal)
  receive (Request s (M.Attack _ _ _)) g = (Invalid NotInPlay s, g)

  -- Reinforce
  receive (Request s (M.Reinforce troopMap)) g@(GamePlay gstate)
    | s == currentPlayer gstate =
      case reinforce troopMap gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (General (Play gstate'), GamePlay gstate')
    | otherwise = (Invalid InvalidMove s, g)

  receive (Request s (M.Reinforce _)) g = (Invalid NotInPlay s, g)

  -- Fortify
  receive (Request s (M.Fortify cFrom cTo nTroops)) g@(GamePlay gstate)
    | s == currentPlayer gstate =
      case fortify cFrom cTo nTroops gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (General (Play gstate'), GamePlay gstate')
    | otherwise = (Invalid InvalidMove s, g)

  receive (Request s (M.Fortify _ _ _)) g = (Invalid NotInPlay s, g)

  -- Invade
  receive (Request s (M.Invade nTroops)) g@(GamePlay gstate)
    | s == currentPlayer gstate =
      case invade nTroops gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (General(Play gstate'), GamePlay gstate')
    | otherwise = (Invalid InvalidMove s, g)
  receive (Request s (Invade _)) g = (Invalid NotInPlay s, g)

  -- ChooseDefenders
  receive (Request s (ChooseDefenders def)) g@(MiniState cAtt cDef att gstate)
    | s == owner gstate cDef =
      case attack cAtt cDef att def gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (General (Play gstate'), GamePlay gstate')
    | otherwise = (Invalid InvalidMove s, g)

  receive (Request s (ChooseDefenders _)) g = (Invalid NotRequestingDefenders s, g)

  -- EndAttack
  receive (Request s EndAttack) g@(GamePlay gstate)
    | s == currentPlayer gstate =
      case endAttack gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (General (Play gstate'), GamePlay gstate')
    | otherwise = (Invalid InvalidMove s, g)
  receive (Request s EndAttack) g = (Invalid InvalidMove s, g)

  -- SkipFortify
  receive (Request s SkipFortify) g@(GamePlay gstate)
    | s == currentPlayer gstate =
      case skipFortify gstate of
        Nothing -> (Invalid InvalidMove s, g)
        Just gstate' -> (General (Play gstate'), GamePlay gstate')
    | otherwise = (Invalid InvalidMove s, g)

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
