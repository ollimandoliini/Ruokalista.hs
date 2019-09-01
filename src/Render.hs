{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where
import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Text.Template
import Text.RawString.QQ
import Data.Maybe


htmlTemplate :: T.Text
htmlTemplate = T.pack
    [r|<html>
        <head>
            <title>Ruokalista</title>
            <meta http-equip="Content-Type" content="text/html" charset="UTF-8">
        </head>
        <body>
            <h1>Lounaspaikat Ruoholahdessa 🍔🍟🍕🌮🥙</h1>
            $name
        </body>
    </html>
    |]


-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = maybe err id . lookup x $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x

renderedHtml content = substitute htmlTemplate helloContext
  where
    helloContext  = context [("name", T.pack $ show content)]



-- parseContent content =
-- map (\x -> (T.pack $ show $ restaurant $ fromJust x) ) content
