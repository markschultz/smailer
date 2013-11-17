module Scheduler where

import Control.Concurrent

testThread n = do
        threadDelay n
        putStrLn $ show n
        return ()
