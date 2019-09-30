{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Requests                    (Date, LaituriCourse, LaituriMenu,
                                              LaituriMeta, NormalizedCourse,
                                              NormalizedMenu, getLaituriMenu,
                                              getMenus)

import           Control.Monad.IO.Class
import           Data.Maybe                  (catMaybes, fromMaybe, listToMaybe)
import           Web.Scotty

import qualified Data.Text.Lazy              as L
import           System.Environment

import           Data.Aeson
import           Data.Time.Calendar
import           Data.Time.Clock
import           GHC.Generics
import           Network.Wai.Middleware.Cors

tupleFromInputString :: String -> Maybe Requests.Date
tupleFromInputString input = if length stringComponents /= 3
  then Nothing
  else Just
    ( toInteger $ read $ head stringComponents
    , read $ stringComponents !! 1
    , read $ stringComponents !! 2
    )
 where
  stringComponents        = words $ replaceDashesWithSpaces input
  replaceDashesWithSpaces = map (\c -> if c == '-' then ' ' else c)


data LaituriMenu = LaituriMenu
    { meta    :: LaituriMeta
    , courses :: [LaituriCourse]
    } deriving (Show, Eq)

getDate :: IO Date
getDate = toGregorian . utctDay <$> getCurrentTime

instance ToJSON NormalizedMenu
instance ToJSON NormalizedCourse


main = do
  port <- fmap (maybe 3001 read) (lookupEnv "PORT")
  scotty port $ do
    middleware simpleCors
    get "/api/:date" $ do
      dateString <- param "date"
      let date =
            fromMaybe <$> getDate <*> (return . tupleFromInputString) dateString
      date'   <- liftIO date
      content <- liftIO $ Requests.getMenus date'
      Web.Scotty.json content
