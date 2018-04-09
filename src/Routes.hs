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
module Routes where

import Web.Scotty
import Data.Text
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

inHandlerDb :: ReaderT
                         SQ.SqlBackend
                         (Control.Monad.Logger.NoLoggingT
                            (ResourceT IO))
                         a
                       -> ActionT
                            LT.Text IO a
inHandlerDb = liftIO . dbFunction

insertDate :: UTCTime -> Transaction -> Transaction
insertDate date (Transaction title amount budgetId _ _) = (Transaction title amount budgetId (Just date) Nothing)

routes :: ScottyM ()
routes = do
  post "/category/:category" $ do
    cat <- param "category"
    getCategory cat
  post "/transaction/:id" $ do
    id <- param "id"
    getTransaction id
  post "/add-category" $ addBudget
  post "/add-transaction" $ addTransaction
  post "/category-transactions/:catId" $ do
    (catId :: Int) <- param "catId"
    month <- liftIO $ getTransactionsInCat catId Month
    json $ month

addTransaction = do
  date <- liftIO $ getCurrentTime
  transaction <- jsonData
  record <- inHandlerDb $ insert $ insertDate date (transaction :: Transaction)
  json record


addBudget = do
  t <- jsonData :: ActionM Budget
  budgetId <- inHandlerDb $ insert t
  json budgetId


getTransaction :: Text -> ActionM ()
getTransaction id = do
  findTransaction <- inHandlerDb $ SQ.get $ SQ.toSqlKey $ read $ T.unpack id
  json $ (findTransaction :: Maybe Transaction)

getCategory :: Text -> ActionM ()
getCategory cat = do
  findPost <- inHandlerDb $ SQ.get (SQ.toSqlKey $ read $ T.unpack cat)
  json $ (findPost :: Maybe Budget)
