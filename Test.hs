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

grep :: String -> String -> Int
grep w "" = 0
grep w s | take l s == w = l
         | length s < l = length s
         | otherwise = 1 + grep w (tail s)
         where l = length w

parenPairs = go 0 []
          where
            go _ _        []         = []
            go j acc      ('{' : cs) =          go (j + 1) (j : acc) cs
            go j []       ('}' : cs) =          go (j + 1) []        cs -- unbalanced parentheses!
            go j (i : is) ('}' : cs) = (i, j) : go (j + 1) is        cs
            go j acc      (c   : cs) =          go (j + 1) acc       cs


extractPhase s = take (snd poss + 1) s'
  where s' = drop (grep "\"phase\":" s) s
        poss = head $ filter ((== 0) . fst) (parenPairs s')


splitOn :: Char -> String  -> [String]
splitOn c s = case dropWhile (\x -> x == c) s of
                    "" -> []
                    s' -> w : splitOn c s''
                          where (w, s'') = break (\x -> x == c) s'

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
    && "\"attackers_remaining\":" ++ show (fromEnum att)`elem` fields
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
