{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module StateMachineSpec (tests) where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Mollie.API.State
import Mollie.API.Types (PaymentStatus(..))
import qualified Data.Time as Time

-- Dummy base data
dummyData :: PaymentData
dummyData = PaymentData "tr_dummy" (Time.UTCTime (toEnum 50000) 0) undefined "Desc" Nothing

-- | Pure transition tests. Notice how these are completely pure and don't need Mocks.
prop_OpenToPending :: Bool
prop_OpenToPending = 
    let initial = PaymentOpen dummyData
        pending = markPending initial
    in getPaymentStatus pending == PaymentPending

-- | Another pure property: if we transition to Paid, the status matches
prop_PendingToPaid :: Bool
prop_PendingToPaid = 
    let pending = PaymentPending dummyData
        paid = markPaid (Time.UTCTime (toEnum 50000) 0) pending
    in getPaymentStatus paid == PaymentPaid

-- In Haskell, proving illegal states representable is generally done simply by building a module
-- and watching it compile! If we wrote:
--
-- illegalTransition :: Payment 'PaymentPaid -> Payment 'PaymentCanceled
-- illegalTransition = cancelPayment
--
-- It would fail to compile!

tests :: TestTree
tests = testGroup "State Machine Properties"
  [ QC.testProperty "Transition Open -> Pending yields correct status" prop_OpenToPending
  , QC.testProperty "Transition Pending -> Paid yields correct status" prop_PendingToPaid
  ]
