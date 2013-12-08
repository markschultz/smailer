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
User
    email String
    password String
    group GroupId Eq default=1
    Email email
    deriving Show

Group
    name String
    admin UserId
    deriving Show
|]

createPool c = createPostgresqlPool c 20

runPool p sql = runSqlPersistMPool sql p

login e p = do
        runMigration migrateAll
        user <- selectFirst [ UserEmail ==. e, UserPassword ==. p] []
        case user of
            Nothing -> return False
            Just _ -> return True

register e p = do
        runMigration migrateAll
        id <- insert $ User e p $ Key {unKey = PersistInt64 1}
        liftIO $ print id
        return id

