{-# LANGUAGE OverloadedStrings #-}

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

{-getTransactionsInCat :: Int -> TimeRange -> IO [Transaction]-}
getTransactionsInCat cat Month = do
  month <- getCurrentMonth
  now <- getCurrentTime
  category <- dbFunction $ SQ.get (SQ.toSqlKey $ (fromIntegral cat :: Int64))
  return $ case category of
    Just x -> dbFunction $ selectList [TransactionBudgetId ==. x] []
    _ -> []
  {-items <- dbFunction $ selectList [TransactionBudgetId ==. category] []-}
  {-return items-}
getTransactionsInCat cat _ = return []

