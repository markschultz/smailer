{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import System.Environment
import Control.Monad

main = do
        port <- liftM read $ getEnv "PORT"
        scotty port $ do
            get "/" $ do
                html "Hello World!"

