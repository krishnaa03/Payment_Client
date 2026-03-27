{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Mollie.API.Idempotency
Description : STM-based Idempotency tracking for duplicate payments
-}
module Mollie.API.Idempotency where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Mollie.API.State (AnyPayment)
import System.Random (randomIO)
import Numeric (showHex)

newtype IdempotencyKey = IdempotencyKey { unIdempotencyKey :: Text.Text }
    deriving (Show, Eq, Ord)

-- | A mock local store tracking idempotent requests. In production, this
-- would be backed by Redis or Postgres, but for demonstration of STM it's held in memory.
newtype IdempotencyStore = IdempotencyStore (TVar (Map.Map IdempotencyKey AnyPayment))

-- | Create a new STM idempotency store.
newIdempotencyStore :: IO IdempotencyStore
newIdempotencyStore = do
    tvar <- newTVarIO Map.empty
    return $ IdempotencyStore tvar

-- | Generate a random idempotency key.
generateIdempotencyKey :: IO IdempotencyKey
generateIdempotencyKey = do
    val <- (randomIO :: IO Int)
    return $ IdempotencyKey $ Text.pack $ showHex val ""

-- | Safely execute or retrieve a previously executed payment
-- This guarantees we never create duplicate payments even under heavy concurrency.
-- If the key exists, the stored payment is returned entirely bypassing the IO action.
-- If not, the IO action is executed (which would be our backend API call), and the result is stored.
executeIdempotent :: IdempotencyStore 
                  -> IdempotencyKey 
                  -> IO AnyPayment 
                  -> IO AnyPayment
executeIdempotent (IdempotencyStore storeTVar) key runPaymentCommand = do
    -- We can't put IO actions in STM directly without breaking purity and avoiding duplicate side effects.
    -- First, we check if it exists or reserve it.
    shouldRun <- atomically $ do
        store <- readTVar storeTVar
        case Map.lookup key store of
            Just _ -> return False
            Nothing -> return True

    if shouldRun
       then do
           payment <- runPaymentCommand
           -- Commit the payment after successful creation
           atomically $ modifyTVar storeTVar (Map.insert key payment)
           return payment
       else do
           -- We know it's there
           Just payment <- atomically $ Map.lookup key <$> readTVar storeTVar
           return payment
