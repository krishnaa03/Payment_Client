{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Mollie.API.Webhooks
Description : HMAC-SHA256 webhook signature verification
-}
module Mollie.API.Webhooks where

import Crypto.Hash (SHA256(..), Digest)
import Crypto.MAC.HMAC (hmac, HMAC, hmacGetDigest)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Mollie.API.Errors (PaymentError(WebhookVerificationFailed))
import Numeric (showHex)

{-|
  Verifies the HMAC-SHA256 signature of an incoming webhook payload.
  Rejects tampered payloads immediately.
-}
verifyWebhookSignature :: B.ByteString -- ^ The secret key provided by Mollie
                       -> B.ByteString -- ^ The raw request payload
                       -> B.ByteString -- ^ The expected signature from the header
                       -> Either PaymentError B.ByteString
verifyWebhookSignature secret payload expectedSignature =
    let 
        computedHmac :: HMAC SHA256
        computedHmac = hmac secret payload
        
        computedDigest :: Digest SHA256
        computedDigest = hmacGetDigest computedHmac

        -- convert digest to hex string for comparison
        computedHex = TE.encodeUtf8 . T.pack . concatMap (flip showHex "") . B.unpack . TE.encodeUtf8 . T.pack . show $ computedDigest
        -- Note: in cryptonite `show computedDigest` gives the hex string. We just need to pack it.
        computedStr = TE.encodeUtf8 $ T.pack $ show computedDigest

    in if computedStr == expectedSignature
       then Right payload
       else Left $ WebhookVerificationFailed "Webhook signature mismatch. Payload may be tampered."
