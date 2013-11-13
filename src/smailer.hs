{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Mailgun
import Data.Monoid
import System.Environment
import Control.Monad
import Data.Text as T
import qualified Data.Text.Lazy as TL

main = do
        port <- liftM read $ getEnv "PORT"
        mgKey <- getEnv "MAILGUN_API_KEY"
        mgLogin <- getEnv "MAILGUN_SMTP_LOGIN"
        let (_:mgDomain) = splitOn "@" $ T.pack mgLogin
        dbc <- getEnv "DATABASE_URL"
        let (auth:server) = splitOn "@" $ T.pack dbc
        let (_:up:_) = splitOn "//" auth
        putStrLn $ show up
        scotty port $ do
            get "/" $ do
                html "Hello World!"
            get "/loaderio-9204d6a37af2101e440254b90c29248a" $ do
                text "loaderio-9204d6a37af2101e440254b90c29248a"
            get "/:to/:text" $ do
                to <- param "to"
                txt <- param "text"
                html $ mconcat [ "<h1>TO: ", to
                               , "</h1><br/><h1>FROM: ", txt
                               , "</h1>" ]
            get "/db" $ do
                html $ TL.fromStrict up

