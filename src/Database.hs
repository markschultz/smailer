{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Database where

import qualified Data.Text as T
import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    email String
    password String
    group GroupId Eq default=1
    Email email
    deriving Show

Group
    name String
    admin UserId Maybe
    deriving Show
|]

getField e ff = ff $ entityVal e

createPool c = createPostgresqlPool c 20

runPool p sql = runSqlPersistMPool sql p

getGroups = do
        groups <- selectList [] []
        let names = map (\e -> (getField e groupName, case (fromPersistValueText $ unKey $ entityKey e) of
                        Left l -> T.pack l
                        Right r -> r)) groups
        return names

login e p = do
        runMigration migrateAll
        user <- selectFirst [ UserEmail ==. e, UserPassword ==. p] []
        case user of
            Nothing -> return False
            Just _ -> return True

register e p g = do
        runMigration migrateAll
        let key = Key {unKey = PersistInt64 g}
        group <- selectFirst [ GroupId ==. key] []
        case group of
            Nothing -> return Nothing
            Just jGroup -> do
                --id2 <- insert $ User e p $ Key {unKey = PersistInt64 1}
                id <- insert $ User e p $ entityKey jGroup
                liftIO $ print id
                return $ Just id

updateGroup uid gid = undefined
