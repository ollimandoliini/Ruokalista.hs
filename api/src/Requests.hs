{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Requests where

import           Data.Aeson
import           Data.Foldable
import           Data.Maybe
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           GHC.Generics         (Generic)
import           Network.HTTP.Conduit (simpleHttp)
import           Prelude.Compat

type Date = (Integer, Int, Int) -- (year, month, day)


getMenus :: Date -> IO [Menu]
getMenus date = do
    let urls = requestUrls date
    mMenu <- mapM getMenuFromUrl urls
    return $ catMaybes mMenu

instance ToJSON Menu
instance ToJSON Course

data Menu = Menu
    { restaurant :: T.Text
    , foods      :: [Course]
    } deriving (Show, Generic)

newtype Course = Course
    { title     :: T.Text
    } deriving (Show, Generic)
    -- , itemPrice :: T.Text

instance FromJSON Menu where
    parseJSON x = asum (map ($ x) [parseLaituri, parseFazer])
        where
        parseLaituri = withObject "menu" $ \o -> do
                        restaurant <- (o .: "meta") >>= (.: "ref_title")
                        courses <- o .: "courses"
                        foods' <- V.toList <$> traverse (.: "title_fi") courses
                        let foods =  Course <$> foods'
                        return Menu{..}

        parseFazer = withObject "menu" $ \o -> do
                        let restaurant = "Fazer Food & Co. Ruoholahti"
                        (setMenus :: [Object]) <-  (o .: "LunchMenu") >>= ( .: "SetMenus")
                        (meals :: [[Object]]) <- traverse (.: "Meals") setMenus
                        (foods' :: [[T.Text]]) <- (traverse . traverse) (.: "Name") meals
                        let foodNames = T.intercalate ", " <$> foods' :: [T.Text]
                        let foods = Course <$> foodNames

                        if not $ null foods then return Menu{..} else fail "no menus"


requestUrls :: Date -> [String]
requestUrls date =
    [laituri, fazer]
    where
        (year, month, day) = date
        laituri = "https://www.sodexo.fi/ruokalistat/output/daily_json/21119/" ++ show year++ "/" ++ show month++ "/" ++ show day ++ "/fi"
        fazer = "https://www.fazerfoodco.fi/api/restaurant/menu/day?date=" ++ show year ++ "/" ++ show month ++ "/" ++ show day ++ "&language=fi&restaurantPageId=163688" :: String

getMenuFromUrl :: String -> IO (Maybe Menu)
getMenuFromUrl url = do
    content <- simpleHttp url
    let decoded = decode content :: Maybe Menu
    return decoded
