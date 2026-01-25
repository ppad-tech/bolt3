{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Keys
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Per-commitment key derivation per BOLT #3.
--
-- Implements key derivation formulas:
--
-- @
-- pubkey = basepoint + SHA256(per_commitment_point || basepoint) * G
-- revocationpubkey = revocation_basepoint * SHA256(revocation_basepoint || per_commitment_point)
--                  + per_commitment_point * SHA256(per_commitment_point || revocation_basepoint)
-- @

module Lightning.Protocol.BOLT3.Keys (
    -- * Per-commitment point derivation
    -- derive_per_commitment_point

    -- * Key derivation
    -- , derive_pubkey
    -- , derive_localpubkey
    -- , derive_local_htlcpubkey
    -- , derive_remote_htlcpubkey
    -- , derive_local_delayedpubkey
    -- , derive_remote_delayedpubkey

    -- * Revocation key derivation
    -- , derive_revocationpubkey
    -- , derive_revocationprivkey

    -- * Per-commitment secret generation
    -- , generate_from_seed
    -- , derive_secret

    -- * Per-commitment secret storage
    -- , SecretStore
    -- , empty_store
    -- , insert_secret
    -- , derive_old_secret

    -- * Commitment number obscuring
    -- , obscured_commitment_number
    -- , commitment_number_from_locktime_sequence
  ) where

import Lightning.Protocol.BOLT3.Types
