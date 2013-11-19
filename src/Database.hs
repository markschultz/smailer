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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TimeEntry
    stamp UTCTime
    deriving Show
|]

insertRow c = withPostgresqlConn c $ runSqlConn $ do
    runMigration migrateAll
    time <- liftIO getCurrentTime
    id <- insert $ TimeEntry time
    liftIO $ print id
