{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Database where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Data.Time
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
import Control.Monad.Logger

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TimeEntry
    stamp UTCTime
    deriving Show
|]

runDB c sql = runResourceT . runNoLoggingT $ withPostgresqlConn c $ runSqlConn sql

insertRow :: (MonadBaseControl IO m, MonadResource m, MonadLogger m) => SqlPersistT m ()
insertRow = do
    runMigration migrateAll
    time <- liftIO getCurrentTime
    id <- insert $ TimeEntry time
    liftIO $ print id