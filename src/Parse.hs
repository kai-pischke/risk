---- Overloads ------------------------------
{-# LANGUAGE OverloadedStrings #-}


module Parse (
    read,
    show
) where


---- Imports --------------------------------
import Message
import Data.Aeson


readRequest :: String -> Maybe Request
readRequest = undefined
showResponse ::  Response -> String


showResponse (General (WaitingRoom ps)) = undefined
showResponse (General (Setup setup)) = undefined
showResponse (General (Play game)) = undefined


showResponse (Special NumDefenders p) = undefined

showResponse (Invalid InvalidMove) = undefined
showResponse (Invalid NotTurn) = undefined
