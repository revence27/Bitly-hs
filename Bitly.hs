module Bitly where

import qualified Data.Map as DM
import JSON
import Network.HTTP
import Network.URI
import Network.URL
import Util

data Bitly = Bitly {login, apiKey :: String}

bitlyHost, version :: String
version = "2.0.1"

bitlyHost = "http://api.bit.ly/"

shorten :: Bitly -> String -> IO String
shorten (Bitly lgn cle) url = case parseAbsoluteURI (bitlyHost ++ "shorten?" ++
    (exportParams [("login", lgn), ("apiKey", cle), ("version", version),
                   ("longUrl", url)])) of
    Just u -> do
        got <- simpleHTTP (Request u GET [] "")
        case got of
            Left e -> fail ("Network problem: " ++ (show e))
            Right (Response _ _ _ b) -> case toJSON b of
                Yes j -> case "errorCode" @@ j of
                    Just (JNum 0) -> case "results" @@ j of
                        Just (JObj mp) -> case DM.keys mp of
                            (k:_) -> case DM.lookup k mp of
                                Just r -> case "shortUrl" @@ r of
                                    Just (JStr sh) -> return sh
                                    _              -> fail "No short URL!"
                                _      -> fail "No results"
                            _     -> fail "No results"
                        _              -> fail "No results."
                    _             -> case "errorMessage" @@ j of
                        Just (JStr e) -> fail e
                        _             -> fail "Bit.ly threw you an error."
                No nj -> fail ("Bad JSON: " ++ nj)
    _      -> fail "Couldn't parse the URL."
