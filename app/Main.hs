module Main where

import Lib
import Web.Scotty
import Routes
import Database

main :: IO ()
main = do
  migrateDB
  scotty 3003 $ do
    routes
