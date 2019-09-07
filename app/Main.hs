{-# LANGUAGE OverloadedStrings #-}
module Main where

import Requests (getLaituriMenu, getMenus, Date, LaituriMenu)
import Render

import Web.Scotty
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad.IO.Class

import qualified Data.Text.Lazy as L
import System.Environment

import Data.Time.Clock
import Data.Time.Calendar


tupleFromInputString :: String -> Maybe Requests.Date
tupleFromInputString input = 
  if length stringComponents /= 3
  then Nothing
  else Just (toInteger (read (stringComponents !! 0)), read (stringComponents !! 1), read (stringComponents !! 2))
  where
    stringComponents = words $ replaceDashesWithSpaces input
    replaceDashesWithSpaces = map (\c -> if c == '-' then ' '; else c)


getDate = getCurrentTime >>= return . toGregorian . utctDay

main = do
  port <- fmap (maybe 3000 read) (lookupEnv "PORT")
  scotty port $ do
    get "/" $ do
      date <- liftIO $ getDate
      content <- liftIO $ Requests.getMenus date
      html $ renderedHtml content

    get "/:date" $ do
      dateString <- param "date"
      let date = fromMaybe (2019, 9, 2) $ tupleFromInputString dateString
      content <- liftIO $ Requests.getMenus date
      html $ renderedHtml content

