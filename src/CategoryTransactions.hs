{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies      #-}

module CategoryTransactions where

import Database
import Database.Persist.Sql
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar
import Data.Int (Int64)
import qualified Data.Text.Internal.Lazy as LT
import qualified Database.Persist.Sql as SQ
import Control.Monad.Trans.Reader
import Control.Monad.Logger
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Resource.Internal
import qualified Data.Text as T
import Web.Scotty.Internal.Types
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

data TimeRange =
    Month
  | Week
  | Day deriving (Show, Eq, Ord)

date :: IO (Integer, Int, Int)
date = do
  now <- getCurrentTime
  return $ toGregorian $ utctDay now

transactionsQuery :: MonadIO m => Int64 -> Int64 -> ReaderT SqlBackend m [Entity Transaction]
transactionsQuery catId month  = rawSql "select ?? from transaction where budget_id=? and extract(month from created) = ?" [PersistInt64 catId, PersistInt64 month]

getTransactionsInCat ::
  Text -> TimeRange -> IO [Entity Transaction]
getTransactionsInCat cat Month = do
  now <- getCurrentTime
  (year, month, _) <- date
  putStrLn . show $ now
  dbFunction' $ selectList
    [ TransactionBudgetId ==. (SQ.toSqlKey $ read $ T.unpack cat)
    , TransactionCreated <=. Just now
    , TransactionCreated >=. Just (UTCTime (fromGregorian year month 1) 0)
    ] []

