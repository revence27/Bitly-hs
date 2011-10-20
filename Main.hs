module Main where

import Bitly
import System.Environment

promptFor :: String -> IO String
promptFor str = do
    putStrLn str
    getLine

main :: IO ()
main = do
    bitlyLogin <- (getEnv "BITLYLOGIN") `catch` (\_ -> promptFor "Bitly login")
    bitlyKey   <- (getEnv "BITLYKEY") `catch` (\_ -> promptFor "Bitly key")
    args <- getArgs
    case args of
        [u]     -> do
            got <- shorten (Bitly bitlyLogin bitlyKey) u
            putStrLn got
        (x:xs) -> do
            withArgs [x] main
            withArgs xs main
        _      -> fail "Provide the URL as an argument."
