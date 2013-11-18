module Scheduler where

import Control.Concurrent
import Data.Time

testThread n = do
        time <- getCurrentTime
        threadDelay n
        time2 <- getCurrentTime
        let diff = diffUTCTime time time2
        putStrLn $ show diff
        return ()
