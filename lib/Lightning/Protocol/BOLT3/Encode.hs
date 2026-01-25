{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Encode
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Serialization for BOLT #3 transactions and scripts.

module Lightning.Protocol.BOLT3.Encode (
    -- * Transaction serialization
    -- encode_tx
    -- , encode_tx_for_signing

    -- * Script serialization
    -- , encode_script
    -- , encode_witness

    -- * Primitive encoding
    -- , encode_varint
    -- , encode_le32
    -- , encode_le64
  ) where

import Lightning.Protocol.BOLT3.Types
