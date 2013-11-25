{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Mailgun
--import Scheduler
import Database
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
import qualified Database.Persist
import qualified Database.Persist.TH
import qualified Database.Persist.Postgresql
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Network.Wai.Middleware.Gzip (gzip,def)
import Network.Wai.Session.ClientSession (clientsessionStore)
import Network.Wai.Session (withSession, Session)
import qualified Data.Vault.Strict as Vault
import Data.Default (def)

blaze = html . renderHtml

getConnectionString t = mconcat ["user=" , u , " password=" , pw ,
    " host=" , h , " port=" , p , " dbname=" , db]
    where
        (auth:server:_) = splitOn "@" t
        (_:up:_) = splitOn "//" auth
        (u:pw:_) = splitOn ":" up
        (hp:db:_) = splitOn "/" server
        (h:p:_) = splitOn ":" hp

main = do
        hSetBuffering stdout LineBuffering
        port <- liftM read $ getEnv "PORT"
        mgKey <- getEnv "MAILGUN_API_KEY"
        mgLogin <- getEnv "MAILGUN_SMTP_LOGIN"
        let (_:mgDomain:_) = splitOn "@" $ T.pack mgLogin
        dbc <- getEnv "DATABASE_URL"
        let dbp = T.pack dbc
        scotty port $ do
            middleware $ gzip def
            get "/" $ html "Hello World!"
            get "/loaderio-9204d6a37af2101e440254b90c29248a" $
                text "loaderio-9204d6a37af2101e440254b90c29248a"
            get "/db" $ text $ TL.fromStrict $ getConnectionString dbp
            get "/db1" $ do
                liftIO $ runDB (T.encodeUtf8 $ getConnectionString dbp) insertRow
                status ok200
            post "/emails" $ do
                f <- param "from"
                s <- param "subject"
                text $ mconcat ["from: " , f , "\nsubject: " , s]
            post "/test-trigger" $
                status status200
            post "/email-trigger" $
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

