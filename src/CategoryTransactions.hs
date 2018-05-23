{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies      #-}

module CategoryTransactions where

import Database
import Database.Persist.Sql
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar
import Data.Time.Calendar.Julian
import Data.Time.Calendar.OrdinalDate (fromSundayStartWeek)
import Data.Int (Int64)
import Data.TimeRange
import qualified Data.Text.Internal.Lazy as LT
import qualified Database.Persist.Sql as SQ
import Control.Monad.Trans.Reader
import Control.Monad.Logger
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource.Internal
import qualified Data.Text as T
import Web.Scotty.Internal.Types
import Web.Scotty (Parsable, parseParam)
import           Control.Monad.IO.Class  (liftIO, MonadIO)
import Data.Text
import Data.Time.Clock

dbFunction' :: ReaderT
                         SQ.SqlBackend
                         (Control.Monad.Logger.NoLoggingT
                            (ResourceT IO))
                         a
                       -> IO a
dbFunction' = liftIO . dbFunction

date :: IO (Integer, Int, Int)
date = toGregorian . utctDay <$> getCurrentTime

transactionsQuery :: MonadIO m => Int64 -> Int64 -> ReaderT SqlBackend m [Entity Transaction]
transactionsQuery catId month  = rawSql "select ?? from transaction where budget_id=? and extract(month from created) = ?" [PersistInt64 catId, PersistInt64 month]

buildTimeRange year month day range = case range of
  Month -> fromGregorian year month 1
  Week  -> fromGregorian year month 15
  Day   -> fromGregorian year month day

getTransactionsInCat ::
  Text -> TimeRange -> IO [Entity Transaction]
getTransactionsInCat cat timeRange = do
  now <- getCurrentTime
  (year, month, day) <- date
  print . show $ fromSundayStartWeek year day 0
  print . show $ dayOfWeek $ fromGregorian year month day
  dbFunction' $ selectList
    [ TransactionBudgetId ==. SQ.toSqlKey (read $ T.unpack cat)
    , TransactionCreated <=. Just now
    , TransactionCreated >=. Just (UTCTime (buildTimeRange year month day timeRange) 0)
    ] []


