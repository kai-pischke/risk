module ParseSpec where
  ---- Imports ---------------------
  import Test.Hspec
  import Test.QuickCheck
  import Message as M
  import Parse
  import Data.ByteString.Lazy.UTF8 (ByteString, fromString)
  import Data.ByteString.Lazy.Char8 (unpack)
  import System.Random
  import GameElements
  import RiskBoard
  import Battle
  import SetupBoard
  import State as S
  import Data.List (isInfixOf)
  import Data.Maybe
  ----------------------------------


  ---- tests ----
  startGameValid = [
    "{\"action\": \"StartGame\", \"sender\": \"Yellow\"}",
    "{\"sender\": \"Blue\", \"action\": \"StartGame\"}"
    ]

  startGameValidDecoded = [
    Request Yellow StartGame,
    Request Blue StartGame
    ]

  startGameInvalid = [
    "{\"action\": \"StartGame\", \"sender\": \"Yelow\"}",
    "{\"action\": \"StartGame\", \"sender\": \"\"}",
    "{\"action\": \"StartGame\", \"sen: \"Blue\"}"
    ]

  placeTroopValid = [
    "{\"action\": \"PlaceTroop\", \"sender\": \"Green\", \"country\": \"Scandinavia\"}",
    "{\"action\": \"PlaceTroop\",\"sender\": \"Blue\",\"country\": \"Ontario\"}",
    "{\"action\": \"PlaceTroop\", \"sender\": \"Red\", \"country\": \"Western United States\"}"
    ]

  placeTroopValidDecoded = [
    Request Green (PlaceTroop Scandinavia),
    Request Blue (PlaceTroop Ontario),
    Request Red (PlaceTroop WesternUnitedStates)
    ]

  placeTroopInvalid = [
    "{\"action\": \"PlaceTroop\", \"sender\": \"Green\", \"country\": \"Scand}",
    "{\"action\": \"PlaceTroop\", \"seer\": \"Blue\", \"country\": \"Ontario\"}",
    "{\"action\": \"PlaceTroop\", \"sender\": \"Red\", \"cotry\": \"Western United States\"}",
    "{\"action\": \"PlaTroop\", \"sender\": \"Red\", \"country\": \"Western United States\"}"
    ]

  attackValid = [
    "{\"action\": \"Attack\",\"sender\": \"Blue\",\"attacking_country\": \"Yakutsk\",\"defending_country\": \"Ontario\",\"number_of_attackers\": 3}",
    "{\"action\": \"Attack\",\"sender\": \"Green\",\"attacking_country\": \"Japan\",\"defending_country\": \"Japan\",\"number_of_attackers\": 1}",
    "{\"action\": \"Attack\",\"sender\": \"Black\",\"attacking_country\": \"Brazil\",\"defending_country\": \"Peru\",\"number_of_attackers\": 2}"
    ]

  attackValidDecoded = [
    Request Blue (M.Attack Yakutsk Ontario ThreeAtt),
    Request Green (M.Attack Japan Japan OneAtt),
    Request Black (M.Attack Brazil Peru TwoAtt)
    ]

  attackInvalid = [
    "{\"action\": \"Attack\",\"sender\": \"Blue\",\"attacking_country\": \"Yakutsk\",\"defending_country\": \"Ontario\",\"number_of_attackers\": 12}",
    "{\"action\": \"Attack\",\"sender\": \"Blue\",\"attacking_country\": \"Yakutsk\",\"defending_country\": \"Ontario\",\"number_of_attackers\": -3}",
    "{\"action\": \"Attack\",\"sender\": \"Blue\",\"attacking_country\": \"Yakutsk\",\"defending_country\": \"Ontao\",\"number_of_attackers\": 3}",
    "{\"action\": \"Attack\",\"sender\": \"Blue\",\"attackingountry\": \"Yakutsk\",\"defending_country\": \"Ontario\",\"number_of_attackers\": 3}",
    "{\"action\": \"Attack\",\"sender\": \"Blue\",\"attacking_country\": \"Yakutsk\",\"defendountry\": \"Ontario\",\"number_of_attackers\": 3}",
    "{\"action\": \"Attack\",\"sender\": \"Blue\",\"attacking_country\": \"Yakutsk\",\"defending_country\": \"Ontario\",\"number_ofers\": 3}",
    "{\"action\": \"Attack\",\"sender\": \"Blue\",\"attacking_country\": \"Yakutsk\",\"defending_country\": \"Onrio\",\"number_of_attackers\": 3}",
    "{\"action\": \"Attack\",\"sender\": \"Ble\",\"attacking_country\": \"Yakutsk\",\"defending_country\": \"Ontario\",\"number_of_attackers\": 3}"
    ]

  reinforceValid = [
    "{\"action\": \"Reinforce\", \"sender\": \"Yellow\", \"troops\": { \"\" }}",
    "{\"action\": \"Reinforce\", \"sender\": \"Blue\", \"troops\": { \"Alaska\": 3}}",
    "{\"action\": \"Reinforce\", \"sender\": \"Green\", \"troops\": { \"Alaska\": 3, \"Congo\":0, \"East Africa\" : 7}}"
    ]

  reinforceValidDecoded = [
    Request Yellow (M.Reinforce []),
    Request Blue (M.Reinforce [(Alaska, 3)]),
    Request Green (M.Reinforce [(Alaska, 3), (Congo,0), (EastAfrica, 7)])
    ]

  reinforceInvalid = [
    "{\"action\": \"Reiorce\", \"sender\": \"Yellow\", \"troops\": { \"\" }}",
    "{\"action\": \"Reinforce\", \"sender\": \"Blue\", \"troops\": { \"Aska\": 3}}",
    "{\"action\": \"Reinforce\", \"sender\": \"Green\", \"troops\": { \"Alaska\": 3, \"East Africa\" : \"20\", \"Congo\":0}}",
    "{\"action\": \"Reinforce\", \"sender\": \"Blue\", \"troops\": { \"Ontario\"}}",
    "{\"action\": \"Reinforce\", \"sender\": \"Blue\", \"tros\": { \"Western Australia\": 3}}"
    ]

  fortifyValid = [
    "{\"action\": \"Fortify\", \"sender\": \"Yellow\", \"from_country\": \"Ontario\", \"to_country\": \"Venezuela\", \"number_of_troops\": 3}",
    "{\"action\": \"Fortify\", \"sender\": \"Green\", \"from_country\": \"Peru\", \"to_country\": \"Brazil\", \"number_of_troops\": 100}",
    "{\"action\":\"Fortify\",\"sender\": \"Yellow\",\"from_country\": \"Quebec\",\"to_country\": \"South Africa\",\"number_of_troops\":0}"
    ]

  fortifyValidDecoded = [
    Request Yellow (M.Fortify Ontario Venezuela 3),
    Request Green (M.Fortify Peru Brazil 100),
    Request Yellow (M.Fortify Quebec SouthAfrica 0)
    ]

  fortifyInvalid = [
    "{\"action\": \"Fortify\", \"sender\": \"Yellow\", \"from_country\": \"Ontario\", \"to_country\": \"Venezuela\", \"numbof_troops\": 3}",
    "{\"action\": \"Fortify\", \"sender\": \"Yellow\", \"from_country\": \"Ontario\", \"to_country\": \"Venezuela\", \"number_of_troops\": \"3\"}",
    "{\"action\": \"Fortify\", \"sender\": \"Yellow\", \"from_country\": \"Ontario\", \"to_country\": \"Veuela\", \"number_of_troops\": 3}",
    "{\"action\": \"Fortify\", \"sender\": \"Yellow\", \"from_country\": \"Ontario\", \"tocountry\": \"Venezuela\", \"number_of_troops\": 3}",
    "{\"action\": \"Fortify\", \"sender\": \"Yellow\", \"from_country\": \"tario\", \"to_country\": \"Venezuela\", \"number_of_troops\": 3}",
    "{\"action\": \"Fortify\", \"sender\": \"Yellow\", \"froountry\": \"Ontario\", \"to_country\": \"Venezuela\", \"number_of_troops\": 3}",
    "{\"action\": \"Fortify\", \"sender\": \"Ylow\", \"from_country\": \"Ontario\", \"to_country\": \"Venezuela\", \"number_of_troops\": 3}",
    "{\"action\": \"Forfy\", \"sender\": \"Yellow\", \"from_country\": \"Ontario\", \"to_country\": \"Venezuela\", \"number_of_troops\": 3}"
    ]

  invadeValid = [
    "{\"action\": \"Invade\", \"sender\": \"Black\", \"number_of_troops\": 6}",
    "{\"action\": \"Invade\", \"sender\": \"Green\", \"number_of_troops\": 100}",
    "{\"action\": \"Invade\", \"sender\": \"Blue\", \"number_of_troops\": 0}"
    ]

  invadeValidDecoded = [
    Request Black (Invade 6),
    Request Green (Invade 100),
    Request Blue (Invade 0)
    ]

  invadeInvalid = [
    "{\"action\": \"Invade\", \"sender\": \"Black\", \"number_of_troops\": \"6\"}",
    "{\"action\": \"Invade\", \"sender\": \"Black\", \"number_of_ps\": 6}",
    "{\"action\": \"Invade\", \"sender\": \"Bck\", \"number_of_troops\": 6}",
    "{\"action\": \"Invade\", nder\": \"Black\", \"number_of_troops\": 6}",
    "{\"action\": \"ade\", \"sender\": \"Black\", \"number_of_troops\": 6}",
    "{\"ion\": \"Invade\", \"sender\": \"Black\", \"number_of_troops\": 6}"
    ]

  chooseDefendersValid = [
    "{\"action\": \"ChooseDefenders\", \"sender\": \"Black\", \"number_of_defenders\": 1}",
    "{\"action\": \"ChooseDefenders\", \"sender\": \"Green\", \"number_of_defenders\": 2}",
    "{\"action\": \"ChooseDefenders\", \"sender\": \"Blue\", \"number_of_defenders\": 1}"
    ]

  chooseDefendersValidDecoded = [
    Request Black (ChooseDefenders OneDef),
    Request Green (ChooseDefenders TwoDef),
    Request Blue  (ChooseDefenders OneDef)
    ]

  chooseDefendersInvalid = [
    "{\"action\": \"ChooseDefenders\", \"sender\": \"Black\", \"number_of_defenders\": \"1\"}",
    "{\"action\": \"ChooseDefenders\", \"sender\": \"Black\", \"number_of_ers\": 2}",
    "{\"action\": \"ChooseDefenders\", \"sender\": \"Bck\", \"number_of_defenders\": 1}",
    "{\"action\": \"ChooseDefenders\", nder\": \"Black\", \"number_of_defenders\": 2}",
    "{\"action\": \"seDefenders\", \"sender\": \"Black\", \"number_of_defenders\": 2}",
    "{\"ion\": \"ChooseDefenders\", \"sender\": \"Black\", \"number_of_defenders\": 1}",
    "{\"action\": \"ChooseDefenders\", \"sender\": \"Blue\", \"number_of_defenders\": 6}",
    "{\"action\": \"ChooseDefenders\", \"sender\": \"Blue\", \"number_of_defenders\": 0}",
    "{\"action\": \"ChooseDefenders\", \"sender\": \"Blue\", \"number_of_defenders\": -12}"
    ]

  endAttackValid = [
    "{\"action\": \"EndAttack\", \"sender\": \"Yellow\"}",
    "{\"action\": \"EndAttack\", \"sender\": \"Blue\"}"
    ]

  endAttackValidDecoded = [
    Request Yellow EndAttack,
    Request Blue EndAttack
    ]

  endAttackInvalid = [
    "{\"action\": \"EndAttack\", \"sender\": \"Ylow\"}",
    "{\"action\": \"EndAttack\", \"sder\": \"Yellow\"}",
    "{\"action\": \"EndAtt, \"sender\": \"Yellow\"}",
    "{\"on\": \"EndAttack\", \"sender\": \"Yellow\"}"
    ]

  skipFortifyValid = [
    "{\"action\": \"SkipFortify\", \"sender\": \"Yellow\"}",
    "{\"action\": \"SkipFortify\", \"sender\": \"Blue\"}"
    ]

  skipFortifyValidDecoded = [
    Request Yellow SkipFortify,
    Request Blue SkipFortify
    ]

  skipFortifyInvalid = [
    "{\"action\": \"SkipFortify\", \"sender\": \"Ylow\"}",
    "{\"action\": \"SkipFortify\", \"sder\": \"Yellow\"}",
    "{\"action\": \"SkipFor, \"sender\": \"Yellow\"}",
    "{\"on\": \"SkipFortify\", \"sender\": \"Yellow\"}"
    ]

  spec :: Spec
  spec = do
    -- Request -------------------------------------
    describe "Decode" $ do
      context "StartGame" $ do
        it "Correctly deals with valid requests" $ do
          map (decodeRequest.fromString) startGameValid `shouldBe` map Left startGameValidDecoded
        it "Correctly deals with invalid requests" $ do
          map (decodeRequest.fromString) startGameInvalid `shouldBe` map (Right . fromString) (replicate (length startGameInvalid) "{\"Invalid JSON\"}")

      context "PlaceTroop" $ do
        it "Correctly deals with valid requests" $ do
          map (decodeRequest.fromString) placeTroopValid `shouldBe` map Left placeTroopValidDecoded
        it "Correctly deals with invalid requests" $ do
          map (decodeRequest.fromString) placeTroopInvalid `shouldBe` map (Right . fromString) (replicate (length placeTroopInvalid) "{\"Invalid JSON\"}")

      context "Attack" $ do
        it "Correctly deals with valid requests" $ do
          map (decodeRequest.fromString) attackValid `shouldBe` map Left attackValidDecoded
        it "Correctly deals with invalid requests" $ do
          map (decodeRequest.fromString) attackInvalid `shouldBe` map (Right . fromString) (replicate (length attackInvalid) "{\"Invalid JSON\"}")

      context "Reinforce" $ do
        it "Correctly deals with valid requests" $ do
          map (decodeRequest.fromString) reinforceValid `shouldBe` map Left reinforceValidDecoded
        it "Correctly deals with invalid requests" $ do
          map (decodeRequest.fromString) reinforceInvalid `shouldBe` map (Right . fromString) (replicate (length reinforceInvalid) "{\"Invalid JSON\"}")

      context "Fortify" $ do
        it "Correctly deals with valid requests" $ do
          map (decodeRequest.fromString) fortifyValid `shouldBe` map Left fortifyValidDecoded
        it "Correctly deals with invalid requests" $ do
          map (decodeRequest.fromString) fortifyInvalid `shouldBe` map (Right . fromString) (replicate (length fortifyInvalid) "{\"Invalid JSON\"}")

      context "Invade" $ do
        it "Correctly deals with valid requests" $ do
          map (decodeRequest.fromString) invadeValid `shouldBe` map Left invadeValidDecoded
        it "Correctly deals with invalid requests" $ do
          map (decodeRequest.fromString) invadeInvalid `shouldBe` map (Right . fromString) (replicate (length invadeInvalid) "{\"Invalid JSON\"}")

      context "ChooseDefenders" $ do
        it "Correctly deals with valid requests" $ do
          map (decodeRequest.fromString) chooseDefendersValid `shouldBe` map Left chooseDefendersValidDecoded
        it "Correctly deals with invalid requests" $ do
          map (decodeRequest.fromString) chooseDefendersInvalid `shouldBe` map (Right . fromString) (replicate (length chooseDefendersInvalid) "{\"Invalid JSON\"}")

      context "EndAttack" $ do
        it "Correctly deals with valid requests" $ do
          map (decodeRequest.fromString) endAttackValid `shouldBe` map Left endAttackValidDecoded
        it "Correctly deals with invalid requests" $ do
          map (decodeRequest.fromString) endAttackInvalid `shouldBe` map (Right . fromString) (replicate (length endAttackInvalid) "{\"Invalid JSON\"}")

      context "SkipFortify" $ do
        it "Correctly deals with valid requests" $ do
          map (decodeRequest.fromString) skipFortifyValid `shouldBe` map Left skipFortifyValidDecoded
        it "Correctly deals with invalid requests" $ do
          map (decodeRequest.fromString) skipFortifyInvalid `shouldBe` map (Right . fromString) (replicate (length skipFortifyInvalid) "{\"Invalid JSON\"}")
    ---------------------------------------------------------

    -- Response ---------------------------------------------
    describe "Encode" $ do
      context "WaitingRoom" $ do
        it "Correctly encodes" $ do
          wellFormedWaitingRoom [] (encodeResponse (General (WaitingRoom []))) `shouldBe` True
          wellFormedWaitingRoom [Green] (encodeResponse (General (WaitingRoom [Green]))) `shouldBe` True
          wellFormedWaitingRoom [Blue, Black, Green] (encodeResponse (General (WaitingRoom [Blue, Black, Green]))) `shouldBe` True
          wellFormedWaitingRoom [Blue, Blue]  (encodeResponse (General (WaitingRoom [Blue, Blue]))) `shouldBe` True

      context "Setup" $ do
        it "Correctly encodes incomplete" $ do
          let state1 = emptyBoard [Yellow, Blue]
          wellFormedSetup (incompleteBoardOwner state1) (setUpTurnOrder state1) (encodeResponse (General (Setup state1))) `shouldBe` True

        it "Correctly encodes partially complete" $ do
          let state2 = placeList [toEnum 0..toEnum 40] (emptyBoard [Yellow, Green, Blue])
          pendingWith ("Need partiallyCompleteBoardOwner")
          --wellFormedSetup (partiallyCompleteBoardOwner state2) [Yellow, Green, Blue] (encodeResponse (General (Setup state2))) `shouldBe` True

        it "Correctly encodes complete" $ do
          let state3 = placeList ([toEnum 0..toEnum 41] ++ [toEnum 0..toEnum 41] ++ [toEnum 0 .. toEnum 20])  (emptyBoard [Red, Green, Blue])
          let toMaybeFunc g c= (Just $ fst (g c), snd (g c))
          wellFormedSetup (toMaybeFunc $ completeBoardOwner state3) [Red, Green, Blue] (encodeResponse (General (Setup state3))) `shouldBe` True

      context "Game" $ do
        it "Correctly encodes in Reinforce" $ do
          wellFormedGame game (encodeResponse (General (Play game))) `shouldBe` True
        it "Correctly encodes in Attack Normal" $ do
          let game' = nextPhase game
          wellFormedGame game' (encodeResponse (General (Play game'))) `shouldBe` True
        it "Correctly encodes in Attack MidBattle" $ do
          let game' =  changeMiniPhase (MidBattle NorthAfrica SouthAfrica ThreeAtt) $ nextPhase game
          wellFormedGame game' (encodeResponse (General (Play game'))) `shouldBe` True
        it "Correctly encodes in Attack WonBattle" $ do
          let game' = changeMiniPhase (WonBattle NorthAfrica SouthAfrica OneAtt) $ nextPhase game
          wellFormedGame game' (encodeResponse (General (Play game'))) `shouldBe` True
        it "Correctly encodes in Fortify" $ do
          let game' = nextPhase $ nextPhase game
          wellFormedGame game' (encodeResponse (General (Play game'))) `shouldBe` True
        it "Correctly encodes on next turn" $ do
          let game' = nextTurn game
          wellFormedGame game' (encodeResponse (General (Play game'))) `shouldBe` True

      context "Error" $ do
        it "Correctly encodes InvalidMove" $ do
          wellFormedError InvalidMove Yellow (encodeResponse (Invalid InvalidMove Yellow)) `shouldBe` True
        it "Correctly encodes NotYourTurn" $ do
          wellFormedError NotYourTurn Yellow (encodeResponse (Invalid NotYourTurn Yellow)) `shouldBe` True
        it "Correctly encodes NotEnoughPlayers" $ do
          wellFormedError NotEnoughPlayers Yellow (encodeResponse (Invalid NotEnoughPlayers Yellow)) `shouldBe` True
        it "Correctly encodes NotInWaitingRoom" $ do
          wellFormedError NotInWaitingRoom Yellow (encodeResponse (Invalid NotInWaitingRoom Yellow)) `shouldBe` True
        it "Correctly encodes SetupComplete" $ do
          wellFormedError SetupComplete Yellow (encodeResponse (Invalid SetupComplete Yellow)) `shouldBe` True
        it "Correctly encodes NotInSetup" $ do
          wellFormedError NotInSetup Yellow (encodeResponse (Invalid NotInSetup Yellow)) `shouldBe` True
        it "Correctly encodes NotInPlay" $ do
          wellFormedError NotInPlay Yellow (encodeResponse (Invalid NotInPlay Yellow)) `shouldBe` True
        it "Correctly encodes NotRequestingDefenders" $ do
          wellFormedError NotRequestingDefenders Yellow (encodeResponse (Invalid NotRequestingDefenders Yellow)) `shouldBe` True

  wellFormedWaitingRoom :: [Player] -> ByteString -> Bool
  wellFormedWaitingRoom players b = head s == '{'
                                    && last s == '}'
                                    && "\"state\":\"WaitingRoom\"" `elem` fields
                                    && "\"kind\":\"State\"" `elem` fields
                                    && extractPlayerList s == jsonPlayerList players
                                    where s = unpack b
                                          fields = splitOn ',' (drop 1 (init s))

  wellFormedSetup :: (Country -> (Maybe Player,Int)) -> [Player] -> ByteString -> Bool
  wellFormedSetup f ps b = validJSONBoard (extractBoard s) f
                         && head s == '{'
                         && last s == '}'
                         && "\"kind\":\"State\"" `elem` fields
                         && "\"state\":\"Setup\"" `elem` fields
                         && extractPlayerList s == jsonPlayerList ps
                        where s = unpack b
                              fields = splitOn ',' (drop 1 (init s))

  wellFormedGame :: GameState -> ByteString -> Bool
  wellFormedGame g b = validJSONBoard (extractBoard s) f
                         && head s == '{'
                         && last s == '}'
                         && "\"kind\":\"State\"" `elem` fields
                         && "\"state\":\"Play\"" `elem` fields
                         && extractPlayerList s == jsonPlayerList ps
                         && validPhase (phase g) (extractPhase s)
                        where s = unpack b
                              ps = turnOrder g
                              fields = splitOn ',' (drop 1 (init s))
                              f c = (Just (owner g c), troops g c)

  wellFormedError :: Error -> Player ->ByteString -> Bool
  wellFormedError e p b = head s == '{'
                                    && last s == '}'
                                    && "\"error\":\"" ++ show e ++ "\"" `elem` fields
                                    && "\"kind\":\"Error\"" `elem` fields
                                    && "\"player\":\"" ++ show p ++ "\"" `elem` fields
                                    where s = unpack b
                                          fields = splitOn ',' (drop 1 (init s))

  splitOn :: Char -> String  -> [String]
  splitOn c s = case dropWhile (\x -> x == c) s of
                      "" -> []
                      s' -> w : splitOn c s''
                            where (w, s'') = break (\x -> x == c) s'

  grep :: String -> String -> Int
  grep w "" = 0
  grep w s | take l s == w = l
           | length s < l = length s
           | otherwise = 1 + grep w (tail s)
           where l = length w

  jsonPlayerList :: [Player] -> String
  jsonPlayerList players = "[" ++ showJSON players ++ "]"
                          where showJSON [] = ""
                                showJSON [p] = "\"" ++ show p ++ "\""
                                showJSON (p:ps) = showJSON [p] ++ "," ++ showJSON ps


  extractPlayerList :: String -> String
  extractPlayerList s = take (grep "]" (drop (grep "\"players\":" s) s)) (drop (grep "\"players\":" s) s)

  placeList :: [Country] -> SetupState -> SetupState
  placeList [] = id
  placeList (c:cs) = (placeList cs) .fromJust . (placeTroop c)

  extractBoard :: String -> String
  extractBoard s = take (snd poss + 1) s'
    where s' = drop (grep "\"board\":" s) s
          poss = head $ filter ((== 0) . fst) (parenPairs s')

  extractPhase :: String -> String
  extractPhase s = take (snd poss + 1) s'
    where s' = drop (grep "\"phase\":" s) s
          poss = head $ filter ((== 0) . fst) (parenPairs s')

  validPhase :: Phase -> String -> Bool
  validPhase (S.Attack (MidBattle catt cdef att)) s =
    head s == '{'
    && last s == '}'
    && "\"kind\":\"MidBattle\"" `elem` fields
    && "\"attacking_country\":\"" ++ show catt ++ "\"" `elem` fields
    && "\"defending_country\":\"" ++ show cdef ++ "\"" `elem` fields
    && "\"attackers\":" ++ show (fromEnum att) `elem` fields
    where fields = splitOn ',' (drop 1 (init s))

  validPhase (S.Attack (WonBattle catt cdef att)) s =
    head s == '{'
    && last s == '}'
    && "\"kind\":\"BattleEnd\"" `elem` fields
    && "\"attacking_country\":\"" ++ show catt ++ "\"" `elem` fields
    && "\"defending_country\":\"" ++ show cdef ++ "\"" `elem` fields
    && "\"attackers_remaining\":" ++ show (fromEnum att) `elem` fields
    where fields = splitOn ',' (drop 1 (init s))

  validPhase (S.Attack Normal) s =
    head s == '{'
    && last s == '}'
    && "\"kind\":\"Simple\"" `elem` fields
    && "\"phase\":\"Attack\"" `elem` fields
    where fields = splitOn ',' (drop 1 (init s))

  validPhase ph s =
    head s == '{'
    && last s == '}'
    && "\"kind\":\"Simple\"" `elem` fields
    && "\"phase\":\"" ++ show ph ++ "\"" `elem` fields
    where fields = splitOn ',' (drop 1 (init s))

  validJSONBoard :: String -> (Country -> (Maybe Player, Int)) -> Bool
  validJSONBoard s f = head s == '{'
                    && last s == '}' && last (init s) == '}'
                    && all id [ (f' c) `isInfixOf` s | c <- [minBound :: Country ..]]
                      where f' c = case f c of
                                   (Nothing, k) -> '\"' : show c ++ "\":{\"owner\":\"Unowned\",\"number_of_troops\":0}"
                                   (Just p, k) -> '\"' : show c ++ "\":{\"owner\":\"" ++ show p ++ "\",\"number_of_troops\":" ++ show k ++ "}"

  c :: Country -> (Player, Int)
  c WesternAustralia  = (Red, 3)
  c EasternAustralia = (Blue, 3)
  c NewGuinea = (Green, 100)
  c Indonesia =  (Red, 3)
  c Siam = (Red, 3)
  c Brazil = (Blue, 3)
  c Peru =  (Blue, 3)
  c _ = (Black, 3)

  game = newGame [Red, Blue, Green] c (mkStdGen 0)

  parenPairs = go 0 []
               where
                 go _ _        []         = []
                 go j acc      ('{' : cs) =          go (j + 1) (j : acc) cs
                 go j []       ('}' : cs) =          go (j + 1) []        cs -- unbalanced parentheses!
                 go j (i : is) ('}' : cs) = (i, j) : go (j + 1) is        cs
                 go j acc      (c   : cs) =          go (j + 1) acc       cs
