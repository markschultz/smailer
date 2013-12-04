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
User
    email String
    password String
    Email email
    deriving Show
|]

createPool c = createPostgresqlPool c 20

runPool p sql = runSqlPersistMPool sql p

insertRow :: (MonadBaseControl IO m, MonadResource m, MonadLogger m) => SqlPersistT m ()
insertRow = do
    runMigration migrateAll
    time <- liftIO getCurrentTime
    id <- insert $ TimeEntry time
    liftIO $ print id

login e p = do
        runMigration migrateAll
        user <- selectFirst [ UserEmail ==. e, UserPassword ==. p] []
        case user of
            Nothing -> return False
            Just _ -> return True

register e p = do
        runMigration migrateAll
        id <- insert $ User e p
        liftIO $ print id
        return id

