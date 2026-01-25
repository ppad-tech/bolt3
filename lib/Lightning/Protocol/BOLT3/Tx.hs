{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Tx
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Transaction assembly for BOLT #3.
--
-- Constructs:
--
-- * Commitment transactions
-- * HTLC-timeout transactions
-- * HTLC-success transactions
-- * Closing transactions

module Lightning.Protocol.BOLT3.Tx (
    -- * Commitment transaction
    -- CommitmentTx(..)
    -- , build_commitment_tx

    -- * HTLC transactions
    -- , HTLCTx(..)
    -- , build_htlc_timeout_tx
    -- , build_htlc_success_tx

    -- * Closing transaction
    -- , ClosingTx(..)
    -- , build_closing_tx
    -- , build_legacy_closing_tx

    -- * Fee calculation
    -- , commitment_fee
    -- , htlc_timeout_fee
    -- , htlc_success_fee

    -- * Trimming
    -- , is_trimmed
    -- , trimmed_htlcs
    -- , untrimmed_htlcs

    -- * Output ordering
    -- , sort_outputs
  ) where

import Lightning.Protocol.BOLT3.Types
