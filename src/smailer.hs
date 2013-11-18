{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Mailgun
--import Scheduler
import Control.Concurrent
import Data.Monoid
import Control.Monad.IO.Class (liftIO)
import System.Environment
import Control.Monad
import Network.HTTP.Types
import Data.Text as T
import Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import System.IO

blaze = html . renderHtml

main = do
        hSetBuffering stdout LineBuffering
        port <- liftM read $ getEnv "PORT"
        mgKey <- getEnv "MAILGUN_API_KEY"
        mgLogin <- getEnv "MAILGUN_SMTP_LOGIN"
        let (_:mgDomain:_) = splitOn "@" $ T.pack mgLogin
        dbc <- getEnv "DATABASE_URL"
        let (auth:server) = splitOn "@" $ T.pack dbc
        let (_:up:_) = splitOn "//" auth
        scotty port $ do
            get "/" $ html "Hello World!"
            get "/loaderio-9204d6a37af2101e440254b90c29248a" $
                text "loaderio-9204d6a37af2101e440254b90c29248a"
            get "/db" $ html $ TL.fromStrict up
            post "/emails" $ do
                f <- param "from"
                s <- param "subject"
                text $ mconcat ["from: " , f , "\nsubject: " , s]
            post "/test-trigger" $ do
                status status200
            post "/email-trigger" $ do
                status status200
            get "/send/:to/:subj" $ do
                subject <- param "subj"
                to <- param "to"
                resp <- liftIO $ sendEmail (to <> "")
                    ("notification@" <> T.encodeUtf8 mgDomain)
                    subject "This is a test email."
                    (B8.pack mgKey) (T.unpack mgDomain)
                blaze $ H.html $ do
                    H.h1 $ H.toHtml $ "Subject: " <>
                        T.decodeLatin1 subject
                    H.h1 $ H.toHtml $ "To: " <> T.decodeLatin1 to
                    H.br
                    H.h1 "Response:"
                    H.p $ H.toHtml $ T.pack resp

