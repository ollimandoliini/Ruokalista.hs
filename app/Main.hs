module Main where

import Lib (getLaituriUrl)

main :: IO ()
main = do
  url <- getLaituriUrl Nothing
  putStrLn url
