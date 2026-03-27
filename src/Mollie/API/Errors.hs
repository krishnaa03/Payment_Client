{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Mollie.API.Errors
Description : Structured Error Handling ADT replacing exceptions.
-}
module Mollie.API.Errors where

import qualified Data.Text as Text
import Mollie.API.Types (PaymentId, ResponseError)
import GHC.Generics (Generic)

{-|
  The core error type for the entire payment system.
  This represents all expected and unexpected failures without using runtime exceptions.
  Every exported function should return 'Either PaymentError a'.
-}
data PaymentError
    = NetworkError Text.Text
    -- ^ Raised when the network connection drops or times out.
    | AuthError Text.Text
    -- ^ Raised for 401 Unauthorized or 403 Forbidden.
    | PaymentNotFound PaymentId
    -- ^ Raised when a payment cannot be found (404).
    | InvalidAmount Text.Text
    -- ^ Raised for 422 Unprocessable Entity related to amounts.
    | RateLimitExceeded Text.Text
    -- ^ Raised for 429 Too Many Requests.
    | ServerFailure Int
    -- ^ Raised for 5xx Server Errors.
    | WebhookVerificationFailed Text.Text
    -- ^ Raised when webhook HMAC SHA-256 signatures do not match.
    | ClientError Int Text.Text
    -- ^ Raised for generic 4xx Client Errors.
    | DecodeError Text.Text
    -- ^ Raised when responses cannot be parsed.
    deriving (Show, Eq, Generic)

{-|
  Convert Servant / Mollie ResponseError into our strongly-typed Domain Error.
  This removes 'ResponseError' from business logic boundaries.
-}
fromResponseError :: ResponseError -> PaymentError
fromResponseError err = case err of
    -- Note: ResponseError constructors need to be imported or matched abstractly if not fully exported
    -- For now, we stub this out depending on ResponseError's definition
    _ -> NetworkError (Text.pack $ show err)
