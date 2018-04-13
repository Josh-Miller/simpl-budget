{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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

getCurrentMonth :: IO Int64
getCurrentMonth = do
  now <- getCurrentTime
  let (_, month, _) = toGregorian $ utctDay now
  return $ (fromIntegral month :: Int64)

transactionsQuery :: MonadIO m => Int64 -> Int64 -> ReaderT SqlBackend m [Entity Transaction]
transactionsQuery catId month  = rawSql "select ?? from transaction where budget_id=? and extract(month from created) = ?" [PersistInt64 catId, PersistInt64 month]

{-getTransactionsInCat :: Text -> TimeRange -> IO [Entity Transaction]-}
getTransactionsInCat cat Month = do
  month <- getCurrentMonth
  now <- getCurrentTime
  return dbFunction' $ selectList [TransactionBudgetId ==. (SQ.toSqlKey $ read $ T.unpack cat)] []
  {-items <- dbFunction $ selectList [TransactionBudgetId ==. category] []-}
  {-return items-}
{-getTransactionsInCat cat _ = return []-}

