{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where
import qualified Data.ByteString.Lazy as S
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as E
import qualified Data.Text.Lazy as L
import Data.Text.Template
import Text.RawString.QQ
import Data.Maybe
import Requests
import Data.List


htmlTemplate :: T.Text
htmlTemplate = T.pack
    [r|<html>
        <head>
            <title>Ruokalista</title>
            <meta http-equip="Content-Type" content="text/html" charset="UTF-8">
        </head>
        <body>
            <h1>Lounaspaikat Ruoholahdessa 🍔🍟🍕🌮🥙</h1>
            <h2>$name</h2> 
            </br>
            $menu
        </body>
    </html>
    |]


-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = maybe err id . lookup x $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x



renderedHtml :: Maybe NormalizedMenu -> L.Text
renderedHtml Nothing = "Not found"
renderedHtml content =
  substitute htmlTemplate $ context [("name", maybe "Not found" (T.pack . restaurant) content)
                                      ,("menu", maybe "Not Found" menuItems content)]

menuItems :: NormalizedMenu -> T.Text
menuItems menu =
  T.pack $ intercalate "</BR></BR>" (map (\x -> intercalate "</BR>" [title x, itemPrice x]) (foods menu))



