{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Lightning.Protocol.BOLT3
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Bitcoin transaction formats for the Lightning Network, per
-- [BOLT #3](https://github.com/lightning/bolts/blob/master/03-transactions.md).
--
-- This module re-exports the public API from submodules:
--
-- * "Lightning.Protocol.BOLT3.Types" - Core types
-- * "Lightning.Protocol.BOLT3.Keys" - Key derivation
-- * "Lightning.Protocol.BOLT3.Scripts" - Script templates
-- * "Lightning.Protocol.BOLT3.Tx" - Transaction assembly
-- * "Lightning.Protocol.BOLT3.Encode" - Serialization
-- * "Lightning.Protocol.BOLT3.Decode" - Parsing
-- * "Lightning.Protocol.BOLT3.Validate" - Validation

module Lightning.Protocol.BOLT3 (
    -- * Re-exports
    module Lightning.Protocol.BOLT3.Types
  ) where

import Lightning.Protocol.BOLT3.Types
