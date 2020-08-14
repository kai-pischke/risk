module Main where

import Server (run)

port :: Int
port = 9600

main :: IO ()
main = do 
    putStrLn $ "Starting Server on port: " ++ show port
    run port
