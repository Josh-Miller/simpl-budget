{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Info where

import Data.Text
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data Budget = Budget {
  category :: Text
  , amount :: Int
} deriving (Show, Generic)

instance ToJSON Budget
instance FromJSON Budget

