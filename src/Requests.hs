{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Requests where

import           Prelude.Compat
import qualified Data.ByteString.Lazy          as B
import           Network.HTTP.Conduit           ( simpleHttp )
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                , decode
                                                , encode
                                                , Value
                                                , defaultOptions
                                                )
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8    as BL
import           GHC.Generics                   ( Generic )
import qualified Data.HashMap.Strict           as HM
import           Data.Maybe
import           Control.Monad.IO.Class





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
    , properties :: Maybe String
    , desc_fi :: String
    , desc_en :: String
    , desc_se :: String
    } deriving (Show, Eq)

data LaituriMenu = LaituriMenu
    { meta :: LaituriMeta
    , courses :: [LaituriCourse]
    } deriving (Show, Eq)

type Date = (Integer, Int, Int) -- (year, month, day)

-- getCurrentDate :: IO (Integer, Int, Int)
-- getCurrentDate = getCurrentTime >>= return . toGregorian . utctDay


getLaituriMenu :: Date -> IO (Maybe LaituriMenu)
getLaituriMenu date = do
    let (year, month, day) = date
    let url =
            "https://www.sodexo.fi/ruokalistat/output/daily_json/21119/"
                ++ show year
                ++ "/"
                ++ show month
                ++ "/"
                ++ show day
                ++ "/fi"

    content <- simpleHttp url
    let decoded = decode content :: Maybe LaituriMenu
    return decoded

$(deriveJSON defaultOptions ''LaituriMenu)
$(deriveJSON defaultOptions ''LaituriMeta)
$(deriveJSON defaultOptions ''LaituriCourse)



-- getMenus :: MonadIO m => Maybe Date -> Maybe (m NormalizedMenu)
getMenus :: Date -> IO [NormalizedMenu]
getMenus date = do
    mMenu <- getLaituriMenu date
    let m = normalizeMenu <$> mMenu
    return $ catMaybes [m]

data NormalizedMenu = NormalizedMenu
    { restaurant :: String
    , foods :: [ NormalizedCourse ]
    } deriving (Show, Eq)

data NormalizedCourse = NormalizedCourse
    { title :: String
    , itemPrice :: String
    -- , itemProperties :: String
    } deriving (Show, Eq)

normalizeMenu :: LaituriMenu -> NormalizedMenu
normalizeMenu menu = NormalizedMenu
    { restaurant = ref_title $ meta menu
    , foods      =
        map (\x -> NormalizedCourse { title = title_fi x, itemPrice = price x })
            $ courses menu
    }

