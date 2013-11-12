{-# LANGUAGE OverloadedStrings #-}
module Mailgun where

import Network
import Network.HTTP.Conduit
import Network.HTTP.Conduit.MultipartFormData
import Data.ByteString

sendEmail :: ByteString -> ByteString -> ByteString -> ByteString -> ByteString -> String -> IO String
sendEmail to from subject txt key domain = withSocketsDo $ do
    req <- parseUrl $ "https://api.mailgun.net/v2/" ++ domain ++ "/messages"
    postReq <- formDataBody [partBS "from" from
                            ,partBS "to" to
                            ,partBS "subject" subject
                            ,partBS "text" txt] (applyBasicAuth "api" key req)

    res <- withManager $ httpLbs postReq
    return (show (responseStatus res) ++ ": " ++ show (responseBody res))
