{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Render where
import qualified Data.ByteString.Lazy          as S
import qualified Data.Text                     as T
import qualified Data.Text.Lazy.Encoding       as E
import qualified Data.Text.Lazy                as L
import           Data.Text.Template
import           Text.RawString.QQ
import           Data.Maybe
import           Requests
import           Data.List

data Props = Props
    { date :: Date
    , menus :: [ NormalizedMenu ]
    } deriving (Show, Eq)

htmlTemplate :: T.Text
htmlTemplate = T.pack [r|<html>
        <head>
            <title>Ruokalista</title>
            <meta http-equip="Content-Type" content="text/html" charset="UTF-8">
        </head>
        <body>
            <h1>Lounaspaikat Ruoholahdessa 🍔🍟🍕🌮🥙</h1>
            <h2>$date</h2>
            $menus
        </body>
    </html>
    |]


-- | Create 'Context' from association list.
context :: [(T.Text, T.Text)] -> Context
context assocs x = fromMaybe err . lookup x $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x



renderedHtml :: Props -> L.Text
-- renderedHtml Nothing = "Not found"
renderedHtml props = substitute htmlTemplate
  $ context [("date", dateText), ("menus", menuText)]
  -- [ ("name", maybe "Not found" (T.pack . restaurant) (menus props))
  -- , ("restaurantName", restaurants (menus props))
























 where
  dateText =
    T.pack
      $ (\(year, month, day) ->
          show day ++ "." ++ show month ++ "." ++ show year
        )
          (date props)
  menuText = restaurants $ menus props

restaurants :: [NormalizedMenu] -> T.Text
restaurants menus = T.pack menuItems
  where menuItems = intercalate "</BR>" (map renderMenuItems menus)

renderMenuItems :: NormalizedMenu -> String
renderMenuItems menu =
  "<h2>" ++ restaurantName ++ "</h2><div>" ++ menuItems ++ "</div>"
 where
  restaurantName = restaurant menu
  menuItems      = intercalate
    "</BR></BR>"
    (map (\x -> intercalate "</BR>" [title x, itemPrice x]) (foods menu))



