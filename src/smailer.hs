{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Data.Time
import Mailgun
import qualified Database as DB
import qualified Html as Html
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
import System.IO
import Control.Monad.Trans.Resource
import Network.Wai.Middleware.Gzip (gzip,def)
import Network.Wai.Session.ClientSession (clientsessionStore)
import Network.Wai.Session
import Network.Wai
import Web.ClientSession (getDefaultKey)
import qualified Data.Vault as Vault
import Data.Default (def)
import Data.String (fromString)
import Web.Cookie
import Data.Maybe

main = do
        hSetBuffering stdout LineBuffering
        port <- liftM read $ getEnv "PORT"
        mgKey <- getEnv "MAILGUN_API_KEY"
        mgLogin <- getEnv "MAILGUN_SMTP_LOGIN"
        let (_:mgDomain:_) = splitOn "@" $ T.pack mgLogin
        dbc <- getEnv "DATABASE_URL"
        let dbp = T.pack dbc
        session <- Vault.newKey
        key <- getDefaultKey
        let store = clientsessionStore key :: SessionStore (ResourceT IO) String String
        pool <- DB.createPool (T.encodeUtf8 $ getConnectionString dbp)
        let rp = DB.runPool pool
        let getVaultS = getVault session

        scotty port $ do
            middleware $ withSession store (fromString "SESSION") def session
            middleware $ gzip def
            get "/" $ do
                req <- request
                text "resp"
                auth <- isLoggedIn $ getVaultS req
                let (sl , si) = fromJust $ Vault.lookup session (vault req)
                t <- liftIO $ runResourceT $ sl "timeout"
                let t1 = fromMaybe "Nothing" t
                case auth of
                    True -> text $ TL.pack ("You are logged in. " ++ t1)
                    False -> text "You are not logged in."
            get "/logout" $ do
                req <- request
                let (sl, si) = getVaultS req
                liftIO $ runResourceT $ si "timeout" ""
                liftIO $ runResourceT $ si "email" ""
                redirect "/"
            get "/register" $ html Html.register
            post "/register" $ do
                e <- param "email"
                p <- param "password"
                resp <- liftIO $ rp $ DB.register e p
                text $ TL.pack $ show $ resp
            get "/login" $ html Html.login
            post "/login" $ do
                e <- param "email"
                p <- param "password"
                req <- request
                let (sl , si) = fromJust $ Vault.lookup session (vault req)
                resp <- liftIO $ rp $ DB.login e p
                case resp of
                    False -> status status403
                    True -> do
                        liftIO $ runResourceT $ si "email" $ e
                        time <- liftIO $ getCurrentTime
                        liftIO $ runResourceT $ si "timeout" $ show $ addUTCTime loginTimeout time
                        text $ TL.pack $ show $ addUTCTime loginTimeout time
            get "/get" $ do
                req <- request
                let (sl , si) = fromJust $ Vault.lookup session (vault req)
                u <- liftIO $ runResourceT $ sl "u"
                text $ TL.pack $ fromMaybe "Nothing" u
            get "/set" $ do
                req <- request
                let (sl , si) = fromJust $ Vault.lookup session (vault req)
                t <- liftIO getCurrentTime
                liftIO $ runResourceT $ si "u" $ show t
                status status200
            get "/db" $ text $ TL.fromStrict $ getConnectionString dbp
            get "/db1" $ do
                liftIO $ rp DB.insertRow
                status status200
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
                html $ Html.sendEmailTest to subject resp
            get "/loaderio-9204d6a37af2101e440254b90c29248a" $
                text "loaderio-9204d6a37af2101e440254b90c29248a"

getConnectionString t = mconcat ["user=" , u , " password=" , pw ,
    " host=" , h , " port=" , p , " dbname=" , db]
    where
        (auth:server:_) = splitOn "@" t
        (_:up:_) = splitOn "//" auth
        (u:pw:_) = splitOn ":" up
        (hp:db:_) = splitOn "/" server
        (h:p:_) = splitOn ":" hp

isLoggedIn (sl,si) = do
        timeout <- liftIO $ runResourceT $ (sl "timeout" :: ResourceT IO (Maybe String))
        time <- liftIO $ getCurrentTime
        case timeout of
            Nothing -> return False
            Just [] -> return False
            Just t -> do
                if time > (read $ t) then do
                    liftIO $ runResourceT $ si "timeout" ""
                    liftIO $ runResourceT $ si "email" ""
                    return False
                else do
                    liftIO $ runResourceT $ si "timeout" $ show $ addUTCTime loginTimeout time
                    return True

getVault session req = fromJust $ Vault.lookup session (vault req)

loginTimeout = 900
