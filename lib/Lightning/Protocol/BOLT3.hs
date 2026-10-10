{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module: Lightning.Protocol.BOLT3
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Bitcoin transaction and script formats for the Lightning Network, per
-- [BOLT #3](https://github.com/lightning/bolts/blob/master/03-transactions.md)
-- (commitment, HTLC and closing transactions, their output scripts and
-- witnesses, per-commitment key derivation, and per-commitment secret
-- generation and storage).
--
-- Amounts, points, hashes and per-commitment secrets are the types of
-- ppad-bolt1's "Lightning.Protocol.BOLT1"; transactions convert to
-- ppad-tx's "Bitcoin.Prim.Tx" for txids, sighashes and serialization.
-- The @option_static_remotekey@ and @option_anchors@ commitment formats
-- are supported ('CommitmentFormat').

module Lightning.Protocol.BOLT3 (
    -- * Commitment transactions
    CommitmentContext(..)
  , CommitmentFormat(..)
  , CommitmentTx(..)
  , CommitmentOutput(..)
  , OutputType(..)
  , build_commitment_tx

    -- ** HTLCs
  , HTLC(..)
  , HTLCDirection(..)

    -- ** Channel parameters
  , CommitmentNumber
  , commitment_number
  , un_commitment_number
  , next_commitment_number
  , ToSelfDelay(..)
  , CltvExpiry(..)
  , DustLimit(..)
  , FeeratePerKw(..)
  , Locktime(..)
  , Sequence(..)

    -- ** Fees and trimming
  , commitment_fee
  , htlc_timeout_fee
  , htlc_success_fee
  , htlc_trim_threshold
  , is_trimmed
  , anchor_output_value

    -- * HTLC transactions
  , HTLCContext(..)
  , HTLCTx(..)
  , build_htlc_tx

    -- * Closing transactions
  , ClosingTx(..)
  , ClosingOutput(..)
  , ClosingContext(..)
  , ClosingOutputs(..)
  , build_closing_tx
  , LegacyClosingContext(..)
  , build_legacy_closing_tx

    -- ** Dust thresholds
  , dust_p2pkh
  , dust_p2sh
  , dust_p2wpkh
  , dust_p2wsh

    -- * Serialization
  , commitment_to_tx
  , htlc_to_tx
  , closing_to_tx
  , encode_commitment_tx
  , encode_htlc_tx
  , encode_closing_tx

    -- * Keys
    -- ** Basepoints and per-commitment points
  , Basepoints(..)
  , RevocationBasepoint(..)
  , PaymentBasepoint(..)
  , DelayedPaymentBasepoint(..)
  , HtlcBasepoint(..)
  , PerCommitmentPoint(..)
  , derive_per_commitment_point
  , derive_per_commitment_point'

    -- ** Public keys
  , CommitmentKeys(..)
  , derive_commitment_keys
  , derive_commitment_keys'
  , RevocationPubkey(..)
  , LocalDelayedPubkey(..)
  , LocalHtlcPubkey(..)
  , RemoteHtlcPubkey(..)
  , RemotePubkey(..)
  , FundingPubkey(..)
  , derive_pubkey
  , derive_revocationpubkey

    -- ** Private keys
  , Seckey
  , seckey
  , un_seckey
  , derive_privkey
  , derive_revocationprivkey

    -- * Per-commitment secrets
  , Seed
  , seed
  , un_seed
  , SecretIndex
  , secret_index
  , un_secret_index
  , commitment_secret_index
  , generate_from_seed

    -- ** Storage
  , SecretStore
  , empty_secret_store
  , insert_secret
  , derive_old_secret
  , secret_store
  , un_secret_store

    -- ** Commitment number obscuring
  , obscured_commitment_number

    -- * Scripts and witnesses
  , Script(..)
  , to_p2wsh

    -- ** Funding output
  , funding_script
  , funding_witness

    -- ** to_local output
  , to_local_script
  , to_local_witness_spend
  , to_local_witness_revoke

    -- ** to_remote output
  , to_remote_witness_script
  , to_remote_script_pubkey
  , to_remote_witness

    -- ** Anchor outputs
  , anchor_script
  , anchor_witness_owner
  , anchor_witness_anyone

    -- ** Offered HTLC outputs
  , offered_htlc_script
  , offered_htlc_witness_preimage
  , offered_htlc_witness_revoke

    -- ** Received HTLC outputs
  , received_htlc_script
  , received_htlc_witness_timeout
  , received_htlc_witness_revoke

    -- ** HTLC transaction inputs
  , htlc_success_witness
  , htlc_timeout_witness
  , remote_htlc_sighash
  ) where

import Lightning.Protocol.BOLT3.Keys
import Lightning.Protocol.BOLT3.Scripts
import Lightning.Protocol.BOLT3.Tx
import Lightning.Protocol.BOLT3.Types
