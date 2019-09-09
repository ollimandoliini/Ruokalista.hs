{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Requests                       ( getLaituriMenu
                                                , getMenus
                                                , Date
                                                , LaituriMenu
                                                , NormalizedMenu
                                                , LaituriMeta
                                                , LaituriCourse
                                                )
import           Render

import           Web.Scotty
import           Data.Maybe                     ( listToMaybe
                                                , fromMaybe
                                                , catMaybes
                                                )
import           Control.Monad.IO.Class

import qualified Data.Text.Lazy                as L
import           System.Environment

import           Data.Time.Clock
import           Data.Time.Calendar




tupleFromInputString' :: String -> Maybe Requests.Date
tupleFromInputString' input = if length stringComponents /= 3
  then Nothing
  else Just
    ( toInteger $ read $ head stringComponents
    , read $ stringComponents !! 1
    , read $ stringComponents !! 2
    )
 where
  stringComponents        = words $ replaceDashesWithSpaces input
  replaceDashesWithSpaces = map (\c -> if c == '-' then ' ' else c)

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
    { meta :: LaituriMeta
    , courses :: [LaituriCourse]
    } deriving (Show, Eq)

getDate :: IO Date
getDate = toGregorian . utctDay <$> getCurrentTime

main = do
  port <- fmap (maybe 3000 read) (lookupEnv "PORT")
  scotty port $ do
    get "/" $ do
      date    <- liftIO getDate
      content <- liftIO $ Requests.getMenus date
      html $ renderedHtml Props { date = date, menus = content }

    get "/:date" $ do
      dateString <- param "date"
      -- let date = fromMaybe getDate $ tupleFromInputString dateString
      let
        date =
          fmap fromMaybe getDate <*> (return . tupleFromInputString) dateString
      date'   <- liftIO date
      content <- liftIO $ Requests.getMenus date'
      html $ renderedHtml Props { date = date', menus = content }

