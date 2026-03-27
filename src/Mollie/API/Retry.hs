{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Mollie.API.Retry
Description : Retry monad transformer with configurable exponential backoff and jitter
-}
module Mollie.API.Retry where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Mollie.API.Errors

-- | Type class for classifying errors at the type level
class IsRetryable e where
    isRetryable :: e -> Bool

-- | Distinguish between retryable and non-retryable errors
instance IsRetryable PaymentError where
    isRetryable (NetworkError _) = True
    isRetryable (RateLimitExceeded _) = True
    isRetryable (ServerFailure _) = True
    isRetryable (DecodeError _) = True
    isRetryable _ = False

-- | Configuration for retries
data RetryConfig = RetryConfig
    { maxRetries :: Int
    , baseDelay  :: Int -- in microseconds
    } deriving (Show, Eq)

defaultRetryConfig :: RetryConfig
defaultRetryConfig = RetryConfig
    { maxRetries = 3
    , baseDelay = 100000 -- 100ms
    }

{-|
  Executes an IO action that returns an Either e a.
  If it fails and `e` is IsRetryable, we wait with exponential backoff + jitter and retry.
-}
retryIO :: (IsRetryable e, MonadIO m) => RetryConfig -> m (Either e a) -> m (Either e a)
retryIO config action = go (maxRetries config) 0
  where
    go retriesLeft attemptCount = do
        res <- action
        case res of
            Right val -> return (Right val)
            Left err ->
                if isRetryable err && retriesLeft > 0
                    then do
                        let delay = (baseDelay config) * (2 ^ attemptCount)
                        jitter <- liftIO $ randomRIO (0, delay `div` 2)
                        liftIO $ threadDelay (delay + jitter)
                        go (retriesLeft - 1) (attemptCount + 1)
                    else return (Left err)
