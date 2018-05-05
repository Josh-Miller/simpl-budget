{-# LANGUAGE OverloadedStrings #-}

module Data.TimeRange where

import Web.Scotty (Parsable, parseParam)

data TimeRange =
    Month
  | Week
  | Day deriving (Show, Eq, Ord)

instance Parsable TimeRange where
  parseParam "month" = Right Month
  parseParam "week" = Right Week
  parseParam "day" = Right Day
  parseParam _ = Left "unable to parse TimeRange"

