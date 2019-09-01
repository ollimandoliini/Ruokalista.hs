{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib
( getLaituriUrl
) where

import Prelude.Compat
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Aeson (FromJSON, ToJSON, decode, encode, Value, defaultOptions)
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as HM
import Data.Maybe



--     jsonURL = "https://www.sodexo.fi/ruokalistat/output/daily_json/21119/2019/08/30/fi"

type Date = (Integer, Int, Int)

getCurrentDate :: IO (Integer, Int, Int) 
getCurrentDate = getCurrentTime >>= return . toGregorian . utctDay

getLaituriUrl :: Maybe Date -> IO String
getLaituriUrl date =
    case date of
        Nothing -> do
            (year, month, day) <- getCurrentDate
            return ("https://www.sodexo.fi/ruokalistat/output/daily_json/21119/" ++ (show year) ++ "/" ++ (show month)  ++ "/" ++ (show day) ++ "/fi")
        Just date ->
            let (year, month, day) = date
            in
            return ("https://www.sodexo.fi/ruokalistat/output/daily_json/21119/" ++ (show year) ++ "/" ++ (show month)  ++ "/" ++ (show day) ++ "/fi")



getLaituriMenu = do
    url <- getLaituriUrl (Just (2019, 9, 2))
    content <- simpleHttp url
    let decoded = decode content :: Maybe LaituriMenu
    return decoded


-- json_data = do
--     json <- getLaituriJSON
--     return json



-- data Menu = Menu 
--     { meta :: Value
--     , courses :: Value 
--     } deriving (Show, Generic)

-- data Menu = Menu 
--     { meta :: Integer
--     , courses :: Integer
--     } deriving (Show, Generic)

-- instance FromJSON Menu

data LaituriMeta = LaituriMeta
    { generated_timestamp :: Integer
    , requested_timestamp :: Integer 
    , ref_url :: String
    , ref_title :: String
    } deriving (Show, Eq)


data LaituriCourse = LaituriCourse
    { title_fi :: String
    , title_en :: String
    , category :: String
    , price :: String
    , properties :: String
    , desc_fi :: String
    , desc_en :: String
    , desc_se :: String
    } deriving (Show, Eq)

data LaituriMenu = LaituriMenu
    { meta :: LaituriMeta
    , courses :: [LaituriCourse]
    } deriving (Show, Eq)

$(deriveJSON defaultOptions ''LaituriMenu)
$(deriveJSON defaultOptions ''LaituriMeta)
$(deriveJSON defaultOptions ''LaituriCourse)



    
