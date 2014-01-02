{-# LANGUAGE OverloadedStrings #-}
module Html where

import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty
import Data.Monoid
import Data.Text.Encoding as T

sendEmailTest to subject resp = renderHtml $ H.html $ do
    H.h1 $ H.toHtml $ "Subject: " <>
        T.decodeLatin1 subject
    H.h1 $ H.toHtml $ "To: " <> T.decodeLatin1 to
    H.br
    H.h1 "Response:"
    H.p $ H.toHtml $ resp

register groups = renderHtml $ H.html $ do
    H.h1 $ "Register"
    H.form ! A.id "register" ! A.method "post" ! A.action "/register" $ do
        H.h3 "Email Address: "
        H.input ! A.type_ "email" ! A.name "email"
        H.br
        H.h3 "Password: "
        H.input ! A.type_ "password" ! A.name "password"
        H.br
        H.h3 "Group: "
        H.select ! A.name "gid" $ do
            mapM_ (\(lbl,id) -> H.option ! A.value (H.toValue id) $ H.toHtml lbl) groups
        H.br
        H.input ! A.type_ "submit"

login = renderHtml $ H.html $ do
    H.h1 $ "Login"
    H.form ! A.id "login" ! A.method "post" ! A.action "/login" $ do
        H.h3 "Email Address: "
        H.input ! A.type_ "email" ! A.name "email"
        H.br
        H.h3 "Password: "
        H.input ! A.type_ "password" ! A.name "password"
        H.br
        H.input ! A.type_ "submit"

settings e g groups = renderHtml $ H.html $ do
    H.h1 "Settings"
    H.br
    H.h2 "Current Settings"
    H.dl $ do
        H.dt "Email Address"
        H.dd e
        H.dt "Group"
        H.dd g
    H.br
    H.h2 "Update Settings"
    H.form ! A.id "settings" ! A.method "post" ! A.action "/settings" $ do
        H.h3 "Update Group"
        H.select ! A.name "gid" $ do
            mapM_ (\(lbl,id) -> H.option ! A.value (H.toValue id) $ H.toHtml lbl) groups
