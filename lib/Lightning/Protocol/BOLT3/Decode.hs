{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Decode
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Parsing for BOLT #3 transactions and scripts.

module Lightning.Protocol.BOLT3.Decode (
    -- * Transaction parsing
    -- decode_tx

    -- * Script parsing
    -- , decode_script
    -- , decode_witness

    -- * Primitive decoding
    -- , decode_varint
    -- , decode_le32
    -- , decode_le64
  ) where

import Lightning.Protocol.BOLT3.Types
