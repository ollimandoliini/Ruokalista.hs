{-# LANGUAGE OverloadedStrings #-}
module Main where

import Requests (getLaituriMenu, getMenus, Date, LaituriMenu)
import Render

import Web.Scotty
import Data.Maybe (listToMaybe)
import Control.Monad.IO.Class

import qualified Data.Text.Lazy as L


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

tupleFromInputString :: String -> Maybe Requests.Date
tupleFromInputString input = 
  if length stringComponents /= 3
  then Nothing
  else Just (toInteger (read (stringComponents !! 0)), read (stringComponents !! 1), read (stringComponents !! 2))
  where 
    stringComponents = wordsWhen (=='-') input




-- getPort :: Either 

main = scotty 3000 $
  get "/:date" $ do
    dateString <- param "date"
    let date = tupleFromInputString dateString
    -- let date = (2019, 9, 2)

    content <- liftIO $ sequence $ fmap Requests.getMenus date
    -- html $ mconcat ["<h1>Result</h1></br>", L.pack $ show content]
    html $ renderedHtml content
