{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Scripts
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Script templates for BOLT #3 transaction outputs.
--
-- Includes witness scripts for:
--
-- * Funding output (2-of-2 multisig)
-- * to_local output (revocable with CSV delay)
-- * to_remote output (P2WPKH or anchored)
-- * Anchor outputs
-- * Offered HTLC outputs
-- * Received HTLC outputs
-- * HTLC-timeout/success output (revocable with delay)

module Lightning.Protocol.BOLT3.Scripts (
    -- * Funding output
    -- funding_script
    -- , funding_witness

    -- * to_local output
    -- , to_local_script
    -- , to_local_witness_spend
    -- , to_local_witness_revoke

    -- * to_remote output
    -- , to_remote_script
    -- , to_remote_witness

    -- * Anchor outputs
    -- , anchor_script
    -- , anchor_witness_owner
    -- , anchor_witness_anyone

    -- * Offered HTLC output
    -- , offered_htlc_script
    -- , offered_htlc_witness_preimage
    -- , offered_htlc_witness_revoke

    -- * Received HTLC output
    -- , received_htlc_script
    -- , received_htlc_witness_timeout
    -- , received_htlc_witness_revoke

    -- * HTLC-timeout/success output (same as to_local)
    -- , htlc_output_script
    -- , htlc_output_witness_spend
    -- , htlc_output_witness_revoke

    -- * P2WSH helpers
    -- , to_p2wsh
    -- , witness_script_hash
  ) where

import Lightning.Protocol.BOLT3.Types
