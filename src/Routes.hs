{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE DataKinds                 #-}
module Routes where

import Web.Scotty
import Data.Text
import Data.TimeRange
import Database
import CategoryTransactions
import qualified Data.Text as T
import qualified Data.Text.Lazy as LLT
import qualified Data.Text.Internal.Lazy as LT
import qualified Database.Persist.Sql as SQ
import Database.Persist.Class (insert)
import           Control.Monad.IO.Class  (liftIO)
import Control.Monad.Trans.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal
import Web.Scotty.Internal.Types
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Generics.Product (field)
import Control.Lens.Getter

inHandlerDb :: ReaderT
                         SQ.SqlBackend
                         (Control.Monad.Logger.NoLoggingT
                            (ResourceT IO))
                         a
                       -> ActionT
                            LT.Text IO a
inHandlerDb = liftIO . dbFunction

insertDate :: UTCTime -> Maybe UTCTime -> Transaction -> Transaction
insertDate now Nothing (Transaction title amount budgetId _ _) = Transaction title amount budgetId (Just now) Nothing
insertDate _ (Just _) y = y

getCreated :: Transaction -> Maybe UTCTime
getCreated = transactionCreated

getTitle x = x ^. field @"transactionTitle"

routes :: ScottyM ()
routes = do
  post "/category/:category" $ do
    cat <- param "category"
    getCategory cat
  post "/transaction/:id" $ do
    id <- param "id"
    getTransaction id
  post "/add-category" addBudget
  post "/add-transaction" addTransaction
  post "/category-transactions/:catId" $ do
    (catId :: Text) <- param "catId"
    (timeRange :: TimeRange) <- param "timeRange" `rescue` \x -> return Month
    liftIO $ print $ show timeRange
    x <- liftIO $ getTransactionsInCat catId timeRange
    liftIO $ print $ Prelude.map (getTitle . SQ.entityVal) (x :: [SQ.Entity Transaction])
    json $ Prelude.map SQ.entityVal (x :: [SQ.Entity Transaction])

addTransaction = do
  date <- liftIO getCurrentTime
  transaction <- jsonData
  liftIO . putStrLn $ show $ getCreated (transaction :: Transaction)
  record <- inHandlerDb $ insert $ insertDate date (getCreated transaction) (transaction :: Transaction)
  json record


addBudget = do
  t <- jsonData :: ActionM Budget
  budgetId <- inHandlerDb $ insert t
  json budgetId


getTransaction :: Text -> ActionM ()
getTransaction id = do
  findTransaction <- inHandlerDb $ SQ.get $ SQ.toSqlKey $ read $ T.unpack id
  json (findTransaction :: Maybe Transaction)

getCategory :: Text -> ActionM ()
getCategory cat = do
  findPost <- inHandlerDb $ SQ.get (SQ.toSqlKey $ read $ T.unpack cat)
  json (findPost :: Maybe Budget)
