module ParseSpec where
  ---- Imports ---------------------
  import Test.Hspec
  import Test.QuickCheck
  import Message
  import Parse
  import Data.ByteString.Lazy.UTF8 (ByteString, fromString)
  import Data.ByteString.Lazy.Char8 (unpack)
  import System.Random
  import GameElements
  import RiskBoard
  import Battle
  import Data.List (isInfixOf)
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
    Request Blue (Attack Yakutsk Ontario ThreeAtt),
    Request Green (Attack Japan Japan OneAtt),
    Request Black (Attack Brazil Peru TwoAtt)
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
    "{\"action\": \"Reinforce\", \"sender\": \"Yellow\", \"troops\": { \'\' }}",
    "{\"action\": \"Reinforce\", \"sender\": \"Blue\", \"troops\": { \"Alaska\": 3}}",
    "{\"action\": \"Reinforce\", \"sender\": \"Green\", \"troops\": { \"Alaska\": 3, \"East Africa\" : 7, \"Congo\":0}}"
    ]

  reinforceValidDecoded = [
    Request Yellow (Reinforce []),
    Request Blue (Reinforce [(Alaska, 3)]),
    Request Green (Reinforce [(Alaska, 3), (EastAfrica, 7), (Congo,0)])
    ]

  reinforceInvalid = [
    "{\"action\": \"Reiorce\", \"sender\": \"Yellow\", \"troops\": { \'\' }}",
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
    Request Yellow (Fortify Ontario Venezuela 3),
    Request Green (Fortify Peru Brazil 100),
    Request Yellow (Fortify Quebec SouthAfrica 0)
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
        it "Correctly encodes" $ do
          pendingWith "Not Yet Testing"

      context "Game" $ do
        it "Correctly encodes" $ do
          pendingWith "Not Yet Testing"

      context "Error" $ do
        it "Correctly encodes" $ do
          pendingWith "Not Yet Testing"

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


  wellFormedWaitingRoom :: [Player] -> ByteString -> Bool
  wellFormedWaitingRoom players b = head s == '{'
                                    && last s == '}'
                                    && "\"state\":\"WaitingRoom\"" `elem` fields
                                    && "\"kind\":\"State\"" `elem` fields
                                    && take (grep "]" (drop (grep "\"players\":" s) s)) (drop (grep "\"players\":" s) s) == jsonPlayerList players
                                    where s = unpack b
                                          fields = splitOn ',' (drop 1 (init s))
