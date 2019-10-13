{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Requests                    (Date, getMenus)

import           Control.Monad.IO.Class      (liftIO)
import           Data.Maybe                  (fromMaybe)
import           Web.Scotty

import           System.Environment

import           Data.Time.Calendar
import           Data.Time.Clock
import           Network.Wai.Middleware.Cors

tupleFromInputString :: String -> Maybe Date
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

getDate :: IO Date
getDate = toGregorian . utctDay <$> getCurrentTime

main :: IO ()
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
