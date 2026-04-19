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
-- = Overview
--
-- This library implements the transaction and script formats defined in
-- BOLT #3, including:
--
-- * Commitment transactions with to_local, to_remote, anchor, and HTLC
--   outputs
-- * HTLC-timeout and HTLC-success second-stage transactions
-- * Closing transactions (legacy and option_simple_close)
-- * Per-commitment key derivation and secret storage
-- * Transaction serialization and parsing
-- * Stateless validation
--
-- = Quick Start
--
-- @
-- import Lightning.Protocol.BOLT3
--
-- -- Build a commitment transaction
-- let ctx = CommitmentContext { ... }
--     tx = build_commitment_tx ctx
--
-- -- Serialize for signing
-- let bytes = encode_tx tx
--
-- -- Validate the transaction
-- case validate_commitment_tx dustLimit features tx of
--   Right () -> putStrLn "Valid"
--   Left err -> print err
-- @
--
-- = Modules
--
-- * "Lightning.Protocol.BOLT3.Types" - Core types (Satoshi, Pubkey, HTLC,
--   etc.)
-- * "Lightning.Protocol.BOLT3.Keys" - Per-commitment key derivation and
--   secret storage
-- * "Lightning.Protocol.BOLT3.Scripts" - Witness script templates (funding,
--   to_local, HTLC, anchor)
-- * "Lightning.Protocol.BOLT3.Tx" - Transaction assembly
-- * "Lightning.Protocol.BOLT3.Encode" - Transaction serialization
-- * "Lightning.Protocol.BOLT3.Decode" - Transaction parsing
-- * "Lightning.Protocol.BOLT3.Validate" - Stateless validation

module Lightning.Protocol.BOLT3 (
    -- * Types
    -- ** Monetary amounts
    Satoshi(..)
  , MilliSatoshi(..)
  , msatToSat
  , satToMsat

    -- ** Keys and points
  , Pubkey(..)
  , pubkey
  , Seckey(..)
  , seckey
  , Point(..)
  , point

    -- ** Hashes
  , PaymentHash(..)
  , paymentHash
  , PaymentPreimage(..)
  , paymentPreimage

    -- ** Transaction primitives
  , TxId(..)
  , mkTxId
  , OutPoint(..)
  , Sequence(..)
  , Locktime(..)

    -- ** Channel parameters
  , CommitmentNumber(..)
  , commitment_number
  , ToSelfDelay(..)
  , CltvExpiry(..)
  , DustLimit(..)
  , FeeratePerKw(..)

    -- ** HTLC types
  , HTLC(..)
  , HTLCDirection(..)

    -- ** Basepoints
  , Basepoints(..)
  , PerCommitmentPoint(..)
  , PerCommitmentSecret(..)
  , perCommitmentSecret
  , RevocationBasepoint(..)
  , PaymentBasepoint(..)
  , DelayedPaymentBasepoint(..)
  , HtlcBasepoint(..)

    -- ** Derived keys
  , LocalPubkey(..)
  , RemotePubkey(..)
  , LocalDelayedPubkey(..)
  , RemoteDelayedPubkey(..)
  , LocalHtlcPubkey(..)
  , RemoteHtlcPubkey(..)
  , RevocationPubkey(..)
  , FundingPubkey(..)

    -- ** Script and witness
  , Script(..)
  , Witness(..)

    -- ** Channel features
  , ChannelFeatures(..)
  , has_anchors

    -- ** Constants
  , commitment_weight_no_anchors
  , commitment_weight_anchors
  , htlc_timeout_weight_no_anchors
  , htlc_timeout_weight_anchors
  , htlc_success_weight_no_anchors
  , htlc_success_weight_anchors
  , htlc_output_weight
  , dust_p2pkh
  , dust_p2sh
  , dust_p2wpkh
  , dust_p2wsh
  , anchor_output_value

    -- * Key derivation
  , derive_per_commitment_point
  , derive_pubkey
  , derive_localpubkey
  , derive_local_htlcpubkey
  , derive_remote_htlcpubkey
  , derive_local_delayedpubkey
  , derive_remote_delayedpubkey
  , derive_revocationpubkey

    -- ** Secret generation
  , generate_from_seed
  , derive_secret

    -- ** Secret storage
  , SecretStore
  , empty_store
  , insert_secret
  , derive_old_secret

    -- ** Commitment number
  , obscured_commitment_number

    -- * Scripts
    -- ** Funding output
  , funding_script
  , funding_witness

    -- ** to_local output
  , to_local_script
  , to_local_witness_spend
  , to_local_witness_revoke

    -- ** to_remote output
  , to_remote_script
  , to_remote_witness

    -- ** Anchor outputs
  , anchor_script
  , anchor_witness_owner
  , anchor_witness_anyone

    -- ** Offered HTLC
  , offered_htlc_script
  , offered_htlc_witness_preimage
  , offered_htlc_witness_revoke

    -- ** Received HTLC
  , received_htlc_script
  , received_htlc_witness_timeout
  , received_htlc_witness_revoke

    -- ** HTLC output (same as to_local)
  , htlc_output_script
  , htlc_output_witness_spend
  , htlc_output_witness_revoke

    -- ** P2WSH helpers
  , to_p2wsh
  , witness_script_hash

    -- * Transaction assembly
    -- ** Commitment transactions
  , CommitmentTx(..)
  , CommitmentContext(..)
  , CommitmentKeys(..)
  , build_commitment_tx

    -- ** HTLC transactions
  , HTLCTx(..)
  , HTLCContext(..)
  , build_htlc_timeout_tx
  , build_htlc_success_tx

    -- ** Closing transactions
  , ClosingTx(..)
  , ClosingContext(..)
  , build_closing_tx
  , build_legacy_closing_tx

    -- ** Transaction outputs
  , TxOutput(..)
  , OutputType(..)

    -- ** Fee calculation
  , commitment_fee
  , commitment_weight
  , htlc_timeout_fee
  , htlc_success_fee

    -- ** Trimming
  , htlc_trim_threshold
  , is_trimmed
  , trimmed_htlcs
  , untrimmed_htlcs

    -- ** Output ordering
  , sort_outputs

    -- * Conversion to ppad-tx
  , commitment_to_tx
  , htlc_to_tx
  , closing_to_tx

    -- * Serialization
  , encode_tx
  , encode_htlc_tx
  , encode_closing_tx
  , encode_tx_for_signing
  , encode_witness
  , encode_funding_witness

    -- * Parsing
  , BT.Tx(..)
  , BT.TxIn(..)
  , BT.TxOut(BT.TxOut)
  , BT.from_bytes
  , decode_tx

    -- * Validation
  , ValidationError(..)
  , validate_commitment_tx
  , validate_commitment_locktime
  , validate_commitment_sequence
  , validate_htlc_tx
  , validate_htlc_timeout_tx
  , validate_htlc_success_tx
  , validate_closing_tx
  , validate_legacy_closing_tx
  , validate_output_ordering
  , validate_dust_limits
  , validate_anchor_outputs
  , validate_commitment_fee
  , validate_htlc_fee
  ) where

import qualified Bitcoin.Prim.Tx as BT
import Lightning.Protocol.BOLT3.Types
import Lightning.Protocol.BOLT3.Keys
import Lightning.Protocol.BOLT3.Scripts
import Lightning.Protocol.BOLT3.Tx
import Lightning.Protocol.BOLT3.Encode
import Lightning.Protocol.BOLT3.Decode
import Lightning.Protocol.BOLT3.Validate
