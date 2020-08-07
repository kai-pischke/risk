---- Overloads ------------------------------
{-# LANGUAGE OverloadedStrings #-}


module Parse (
    readRequest,
    showResponse
) where


---- Imports --------------------------------
import Message
import Data.Aeson


readRequest :: String -> Maybe Request
readRequest = undefined
showResponse ::  Response -> String

instance ToJSON Response where
    toJSON (General (WaitingRoom ps)) = object [""]

    toJSON (General (Setup setup)) = undefined
    toJSON (General (Play game)) = undefined


    toJSON (Special NumDefenders p) = undefined

    toJSON (Invalid InvalidMove) = undefined
    toJSON (Invalid NotTurn) = undefined
