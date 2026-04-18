{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Decode
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Parsing for BOLT #3 transactions.
--
-- Delegates to ppad-tx for transaction decoding.

module Lightning.Protocol.BOLT3.Decode (
    decode_tx
  ) where

import qualified Bitcoin.Prim.Tx as BT
import qualified Data.ByteString as BS

-- | Decode a raw Bitcoin transaction from bytes.
--
-- Handles both legacy and SegWit transaction formats.
--
-- >>> decode_tx rawTxBytes
-- Just (Tx {...})
decode_tx :: BS.ByteString -> Maybe BT.Tx
decode_tx = BT.from_bytes
