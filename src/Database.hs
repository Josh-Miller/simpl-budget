{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric #-}

module Database where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.TH
import Database.Persist.Postgresql
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Resource.Internal
import Control.Monad.IO.Unlift
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

connStr :: ConnectionString
connStr = "host=localhost dbname=budget user=budget password='' port=5432"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Budget
    name String
    amount Double
    deriving Show Generic
Transaction
    title String
    amount Double
    budgetId BudgetId
    BudgetIdForTransaction budgetId
    deriving Show Generic
|]

instance ToJSON Budget
instance FromJSON Budget

instance ToJSON Transaction
instance FromJSON Transaction

dbFunction ::
  (BaseBackend backend ~ SqlBackend, IsPersistBackend backend,
   MonadUnliftIO m) =>
  ReaderT
    backend
    (NoLoggingT
       (ResourceT
          IO))
    a
  -> m a
dbFunction query = runStderrLoggingT $
        withPostgresqlPool connStr 10 $
        \pool -> liftIO $ runSqlPersistMPool query pool

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: IO ()
migrateDB = runAction connStr (runMigration migrateAll)
