{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Validate
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Stateless validation for BOLT #3 transactions.

module Lightning.Protocol.BOLT3.Validate (
    -- * Validation errors
    -- ValidationError(..)

    -- * Commitment transaction validation
    -- , validate_commitment_tx

    -- * HTLC transaction validation
    -- , validate_htlc_tx

    -- * Closing transaction validation
    -- , validate_closing_tx

    -- * Output validation
    -- , validate_output_ordering
    -- , validate_dust_limits
  ) where

import Lightning.Protocol.BOLT3.Types
