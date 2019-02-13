{-# LANGUAGE OverloadedStrings #-}

module Data.TimeRange where

import Web.Scotty (Parsable, parseParam)
import Data.Time.Clock (UTCTime)
import Data.Time.Format
import Data.Text.Lazy (unpack)

data TimeRange =
    Month
  | Week
  | Day deriving (Show, Eq, Ord)

instance Parsable TimeRange where
  parseParam "month" = Right Month
  parseParam "week" = Right Week
  parseParam "day" = Right Day
  parseParam _ = Left "unable to parse TimeRange"


{-data SomeTime = UTCTime-}

{-instance Parsable SomeTime where-}
  {-parseParam x = parseTimeOrError False defaultTimeLocale "Y-m-d" (unpack x)-}
