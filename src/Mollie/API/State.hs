{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Mollie.API.State
Description : Type-safe Payment state machine
-}
module Mollie.API.State where

import Mollie.API.Types (PaymentStatus(..), PaymentId, Amount, PaymentMethod)
import qualified Data.Time as Time
import qualified Data.Text as Text

{-|
  Base payment data excluding the status which is tracked at the type level.
-}
data PaymentData = PaymentData
    { pdId               :: PaymentId
    , pdCreatedAt        :: Time.UTCTime
    , pdAmount           :: Amount
    , pdDescription      :: Text.Text
    , pdMethod           :: Maybe PaymentMethod
    } deriving (Show, Eq)

{-|
  Type-safe Payment GADT.
  The state is tracked at the type level using the 'PaymentStatus' DataKind.
  This makes illegal state transitions impossible to represent by construction.
-}
data Payment (s :: PaymentStatus) where
    PaymentOpen     :: PaymentData -> Payment 'PaymentOpen
    PaymentPending  :: PaymentData -> Payment 'PaymentPending
    PaymentPaid     :: PaymentData -> Time.UTCTime -> Payment 'PaymentPaid
    PaymentFailed   :: PaymentData -> Time.UTCTime -> Payment 'PaymentFailed
    PaymentCanceled :: PaymentData -> Time.UTCTime -> Payment 'PaymentCanceled
    PaymentExpired  :: PaymentData -> Time.UTCTime -> Payment 'PaymentExpired

deriving instance Show (Payment s)

{-|
  Existential wrapper for a Payment whose state is only known at runtime.
-}
data AnyPayment where
    AnyPayment :: Payment s -> AnyPayment

deriving instance Show AnyPayment

-- | Extract the underlying data from any payment
getPaymentData :: Payment s -> PaymentData
getPaymentData (PaymentOpen d) = d
getPaymentData (PaymentPending d) = d
getPaymentData (PaymentPaid d _) = d
getPaymentData (PaymentFailed d _) = d
getPaymentData (PaymentCanceled d _) = d
getPaymentData (PaymentExpired d _) = d

-- | Extract dynamic status
getPaymentStatus :: Payment s -> PaymentStatus
getPaymentStatus (PaymentOpen _) = PaymentOpen
getPaymentStatus (PaymentPending _) = PaymentPending
getPaymentStatus (PaymentPaid _ _) = PaymentPaid
getPaymentStatus (PaymentFailed _ _) = PaymentFailed
getPaymentStatus (PaymentCanceled _ _) = PaymentCanceled
getPaymentStatus (PaymentExpired _ _) = PaymentExpired

-- | State machine transitions
-- Transitions are pure functions. Illegal transitions (e.g. Paid -> Canceled) are caught at compile time.

markPending :: Payment 'PaymentOpen -> Payment 'PaymentPending
markPending (PaymentOpen d) = PaymentPending d

markPaid :: Time.UTCTime -> Payment 'PaymentPending -> Payment 'PaymentPaid
markPaid t (PaymentPending d) = PaymentPaid d t

failPayment :: Time.UTCTime -> Payment 'PaymentPending -> Payment 'PaymentFailed
failPayment t (PaymentPending d) = PaymentFailed d t

cancelPayment :: Time.UTCTime -> Payment 'PaymentOpen -> Payment 'PaymentCanceled
cancelPayment t (PaymentOpen d) = PaymentCanceled d t

expirePayment :: Time.UTCTime -> Payment 'PaymentOpen -> Payment 'PaymentExpired
expirePayment t (PaymentOpen d) = PaymentExpired d t
