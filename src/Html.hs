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

register = renderHtml $ H.html $ do
    H.h1 $ "Register"
    H.form ! A.id "register" ! method "post" ! action "/register" $ do
        H.h3 "Email Address: "
        H.input ! type_ "text" ! name "email"
        H.br
        H.h3 "Password: "
        H.input ! type_ "password" ! name "password"
        H.br
        H.input ! type_ "submit"
