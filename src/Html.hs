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
