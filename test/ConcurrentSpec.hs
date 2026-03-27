{-# LANGUAGE OverloadedStrings #-}

module ConcurrentSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Mollie.API.Idempotency
import Mollie.API.State
import Mollie.API.Types (PaymentStatus(..))
import Control.Concurrent
import Control.Monad (replicateM_, forM)
import Data.IORef
import qualified Data.Time as Time
import qualified Data.Text as Text

-- | A dummy payment for testing
dummyPayment :: Time.UTCTime -> AnyPayment
dummyPayment t = AnyPayment $ PaymentOpen $ PaymentData 
    { pdId = "tr_test"
    , pdCreatedAt = t
    , pdAmount = undefined
    , pdDescription = "Test"
    , pdMethod = Nothing
    }

tests :: TestTree
tests = testGroup "Concurrency and Idempotency"
  [ testCase "100 concurrent requests should only execute the API call once" testSTMIdempotency
  ]

testSTMIdempotency :: Assertion
testSTMIdempotency = do
    store <- newIdempotencyStore
    key <- generateIdempotencyKey
    
    -- We want to track exactly how many times the "API" actually executes
    apiExecutionCount <- newIORef (0 :: Int)
    
    currentTime <- Time.getCurrentTime
    let mockApiCall = do
            modifyIORef' apiExecutionCount (+1)
            -- Simulate network delay
            threadDelay 50000 
            return $ dummyPayment currentTime

    -- Start 100 concurrent threads trying to create the payment with the same key
    mvars <- forM [1..100] $ \_ -> do
        mvar <- newEmptyMVar
        _ <- forkIO $ do
            _ <- executeIdempotent store key mockApiCall
            putMVar mvar ()
        return mvar

    -- Wait for all 100 threads to finish
    mapM_ takeMVar mvars
    
    finalCount <- readIORef apiExecutionCount
    
    assertEqual "The API mock should only have been called exactly once despite 100 concurrent attempts" 1 finalCount
