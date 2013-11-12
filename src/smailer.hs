{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Mailgun
import Data.Monoid
import System.Environment
import Control.Monad
import Data.Text as T

main = do
        port <- liftM read $ getEnv "PORT"
        mgKey <- getEnv "MAILGUN_API_KEY"
        mgLogin <- getEnv "MAILGUN_SMTP_LOGIN"
        let (_:mgDomain) = splitOn "@" $ T.pack mgLogin
        scotty port $ do
            get "/" $ do
                html "Hello World!"
            get "/:to/:text" $ do
                to <- param "to"
                text' <- param "text"
                html $ mconcat ["<h1>TO:", to, "</h1><br/><h1>", text', "</h1>"]

