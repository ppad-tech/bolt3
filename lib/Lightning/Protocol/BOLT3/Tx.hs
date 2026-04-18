{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

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
    CommitmentTx(..)
  , CommitmentContext(..)
  , CommitmentKeys(..)
  , build_commitment_tx

    -- * HTLC transactions
  , HTLCTx(..)
  , HTLCContext(..)
  , build_htlc_timeout_tx
  , build_htlc_success_tx

    -- * Closing transaction
  , ClosingTx(..)
  , ClosingContext(..)
  , build_closing_tx
  , build_legacy_closing_tx

    -- * Conversion to ppad-tx
  , commitment_to_tx
  , htlc_to_tx
  , closing_to_tx

    -- * Transaction outputs
  , TxOutput(..)
  , OutputType(..)

    -- * Fee calculation
  , commitment_fee
  , htlc_timeout_fee
  , htlc_success_fee
  , commitment_weight

    -- * Trimming
  , is_trimmed
  , trimmed_htlcs
  , untrimmed_htlcs
  , htlc_trim_threshold

    -- * Output ordering
  , sort_outputs
  ) where

import qualified Bitcoin.Prim.Tx as BT
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT3.Keys
import Lightning.Protocol.BOLT3.Scripts
import Lightning.Protocol.BOLT3.Types

-- transaction outputs ---------------------------------------------------------

-- | Type of output in a commitment transaction.
data OutputType
  = OutputToLocal
  | OutputToRemote
  | OutputLocalAnchor
  | OutputRemoteAnchor
  | OutputOfferedHTLC  {-# UNPACK #-} !CltvExpiry
  | OutputReceivedHTLC {-# UNPACK #-} !CltvExpiry
  deriving (Eq, Show, Generic)

-- | A transaction output with value, script, and type information.
data TxOutput = TxOutput
  { txout_value     :: {-# UNPACK #-} !Satoshi
  , txout_script    :: !Script
  , txout_type      :: !OutputType
  } deriving (Eq, Show, Generic)

-- commitment transaction ------------------------------------------------------

-- | Derived keys needed for commitment transaction outputs.
data CommitmentKeys = CommitmentKeys
  { ck_revocation_pubkey   :: !RevocationPubkey
  , ck_local_delayed       :: !LocalDelayedPubkey
  , ck_local_htlc          :: !LocalHtlcPubkey
  , ck_remote_htlc         :: !RemoteHtlcPubkey
  , ck_local_payment       :: !LocalPubkey
  , ck_remote_payment      :: !RemotePubkey
  , ck_local_funding       :: !FundingPubkey
  , ck_remote_funding      :: !FundingPubkey
  } deriving (Eq, Show, Generic)

-- | Context for building a commitment transaction.
data CommitmentContext = CommitmentContext
  { cc_funding_outpoint    :: !OutPoint
  , cc_commitment_number   :: !CommitmentNumber
  , cc_local_payment_bp    :: !PaymentBasepoint
  , cc_remote_payment_bp   :: !PaymentBasepoint
  , cc_to_self_delay       :: !ToSelfDelay
  , cc_dust_limit          :: !DustLimit
  , cc_feerate             :: !FeeratePerKw
  , cc_features            :: !ChannelFeatures
  , cc_is_funder           :: !Bool
  , cc_to_local_msat       :: !MilliSatoshi
  , cc_to_remote_msat      :: !MilliSatoshi
  , cc_htlcs               :: ![HTLC]
  , cc_keys                :: !CommitmentKeys
  } deriving (Eq, Show, Generic)

-- | A commitment transaction.
data CommitmentTx = CommitmentTx
  { ctx_version            :: {-# UNPACK #-} !Word32
  , ctx_locktime           :: !Locktime
  , ctx_input_outpoint     :: !OutPoint
  , ctx_input_sequence     :: !Sequence
  , ctx_outputs            :: ![TxOutput]
  , ctx_funding_script     :: !Script
  } deriving (Eq, Show, Generic)

-- | Build a commitment transaction.
--
-- Follows the algorithm from BOLT #3:
--
-- 1. Initialize input and locktime with obscured commitment number
-- 2. Calculate which HTLCs are trimmed
-- 3. Calculate base fee and subtract from funder
-- 4. Add untrimmed HTLC outputs
-- 5. Add to_local output if above dust
-- 6. Add to_remote output if above dust
-- 7. Add anchor outputs if option_anchors
-- 8. Sort outputs per BIP69+CLTV
build_commitment_tx :: CommitmentContext -> CommitmentTx
build_commitment_tx ctx =
  let !obscured = obscured_commitment_number
        (cc_local_payment_bp ctx)
        (cc_remote_payment_bp ctx)
        (cc_commitment_number ctx)

      -- Locktime: upper 8 bits are 0x20, lower 24 bits are lower 24 of obscured
      !locktime = Locktime $
        (0x20 `shiftL` 24) .|. (fromIntegral obscured .&. 0x00FFFFFF)

      -- Sequence: upper 8 bits are 0x80, lower 24 bits are upper 24 of obscured
      !inputSeq = Sequence $
        (0x80 `shiftL` 24) .|.
        (fromIntegral (obscured `shiftR` 24) .&. 0x00FFFFFF)

      -- Funding script for witness
      !fundingScript = funding_script
        (ck_local_funding $ cc_keys ctx)
        (ck_remote_funding $ cc_keys ctx)

      -- Calculate untrimmed HTLCs
      !untrimmedHtlcs = untrimmed_htlcs
        (cc_dust_limit ctx)
        (cc_feerate ctx)
        (cc_features ctx)
        (cc_htlcs ctx)

      -- Calculate base fee
      !baseFee = commitment_fee
        (cc_feerate ctx)
        (cc_features ctx)
        (fromIntegral $ length untrimmedHtlcs)

      -- Anchor cost if applicable
      !anchorCost = if has_anchors (cc_features ctx)
        then 2 * anchor_output_value
        else Satoshi 0

      -- Subtract fees and anchors from funder
      !totalDeduction = baseFee + anchorCost
      !(toLocalSat, toRemoteSat) = if cc_is_funder ctx
        then
          let !local = msat_to_sat (cc_to_local_msat ctx)
              !deducted = if unSatoshi local >= unSatoshi totalDeduction
                          then Satoshi (unSatoshi local - unSatoshi totalDeduction)
                          else Satoshi 0
          in (deducted, msat_to_sat (cc_to_remote_msat ctx))
        else
          let !remote = msat_to_sat (cc_to_remote_msat ctx)
              !deducted = if unSatoshi remote >= unSatoshi totalDeduction
                          then Satoshi (unSatoshi remote - unSatoshi totalDeduction)
                          else Satoshi 0
          in (msat_to_sat (cc_to_local_msat ctx), deducted)

      !dustLimit = unDustLimit (cc_dust_limit ctx)

      -- Build HTLC outputs
      !htlcOutputs = map (htlcOutput ctx) untrimmedHtlcs

      -- Build to_local output if above dust
      !toLocalOutput =
        if unSatoshi toLocalSat >= unSatoshi dustLimit
        then
          let !script = to_p2wsh $ to_local_script
                (ck_revocation_pubkey $ cc_keys ctx)
                (cc_to_self_delay ctx)
                (ck_local_delayed $ cc_keys ctx)
          in [TxOutput toLocalSat script OutputToLocal]
        else []

      -- Build to_remote output if above dust
      !toRemoteOutput =
        if unSatoshi toRemoteSat >= unSatoshi dustLimit
        then
          let !script = if has_anchors (cc_features ctx)
                then to_p2wsh $ to_remote_script
                       (ck_remote_payment $ cc_keys ctx)
                       (cc_features ctx)
                else to_remote_script
                       (ck_remote_payment $ cc_keys ctx)
                       (cc_features ctx)
          in [TxOutput toRemoteSat script OutputToRemote]
        else []

      -- Build anchor outputs if option_anchors
      !hasUntrimmedHtlcs = not (null untrimmedHtlcs)
      !toLocalExists = not (null toLocalOutput)
      !toRemoteExists = not (null toRemoteOutput)

      !localAnchorOutput =
        if has_anchors (cc_features ctx) &&
           (toLocalExists || hasUntrimmedHtlcs)
        then
          let !script = to_p2wsh $ anchor_script
                (ck_local_funding $ cc_keys ctx)
          in [TxOutput anchor_output_value script OutputLocalAnchor]
        else []

      !remoteAnchorOutput =
        if has_anchors (cc_features ctx) &&
           (toRemoteExists || hasUntrimmedHtlcs)
        then
          let !script = to_p2wsh $ anchor_script
                (ck_remote_funding $ cc_keys ctx)
          in [TxOutput anchor_output_value script OutputRemoteAnchor]
        else []

      -- Combine and sort all outputs
      !allOutputs = toLocalOutput ++ toRemoteOutput ++
                    localAnchorOutput ++ remoteAnchorOutput ++
                    htlcOutputs
      !sortedOutputs = sort_outputs allOutputs

  in CommitmentTx
       { ctx_version = 2
       , ctx_locktime = locktime
       , ctx_input_outpoint = cc_funding_outpoint ctx
       , ctx_input_sequence = inputSeq
       , ctx_outputs = sortedOutputs
       , ctx_funding_script = fundingScript
       }
{-# INLINE build_commitment_tx #-}

-- | Build an HTLC output for commitment transaction.
htlcOutput :: CommitmentContext -> HTLC -> TxOutput
htlcOutput ctx htlc =
  let !amountSat = msat_to_sat (htlc_amount_msat htlc)
      !keys = cc_keys ctx
      !features = cc_features ctx
      !expiry = htlc_cltv_expiry htlc
  in case htlc_direction htlc of
       HTLCOffered ->
         let !script = to_p2wsh $ offered_htlc_script
               (ck_revocation_pubkey keys)
               (ck_remote_htlc keys)
               (ck_local_htlc keys)
               (htlc_payment_hash htlc)
               features
         in TxOutput amountSat script (OutputOfferedHTLC expiry)
       HTLCReceived ->
         let !script = to_p2wsh $ received_htlc_script
               (ck_revocation_pubkey keys)
               (ck_remote_htlc keys)
               (ck_local_htlc keys)
               (htlc_payment_hash htlc)
               expiry
               features
         in TxOutput amountSat script (OutputReceivedHTLC expiry)
{-# INLINE htlcOutput #-}

-- HTLC transactions -----------------------------------------------------------

-- | Context for building HTLC transactions.
data HTLCContext = HTLCContext
  { hc_commitment_txid     :: !TxId
  , hc_output_index        :: {-# UNPACK #-} !Word32
  , hc_htlc                :: !HTLC
  , hc_to_self_delay       :: !ToSelfDelay
  , hc_feerate             :: !FeeratePerKw
  , hc_features            :: !ChannelFeatures
  , hc_revocation_pubkey   :: !RevocationPubkey
  , hc_local_delayed       :: !LocalDelayedPubkey
  } deriving (Eq, Show, Generic)

-- | An HTLC transaction (timeout or success).
data HTLCTx = HTLCTx
  { htx_version            :: {-# UNPACK #-} !Word32
  , htx_locktime           :: !Locktime
  , htx_input_outpoint     :: !OutPoint
  , htx_input_sequence     :: !Sequence
  , htx_output_value       :: !Satoshi
  , htx_output_script      :: !Script
  } deriving (Eq, Show, Generic)

-- | Internal helper for HTLC transaction construction.
--
-- Both HTLC-timeout and HTLC-success transactions share the same
-- structure, differing only in locktime and fee calculation.
build_htlc_tx_common
  :: HTLCContext
  -> Locktime           -- ^ Transaction locktime
  -> Satoshi            -- ^ Fee to subtract from output
  -> HTLCTx
build_htlc_tx_common ctx locktime fee =
  let !amountSat = msat_to_sat (htlc_amount_msat $ hc_htlc ctx)
      !outputValue = if unSatoshi amountSat >= unSatoshi fee
                     then Satoshi (unSatoshi amountSat - unSatoshi fee)
                     else Satoshi 0
      !inputSeq = if has_anchors (hc_features ctx)
                   then Sequence 1
                   else Sequence 0
      !outpoint = OutPoint (hc_commitment_txid ctx) (hc_output_index ctx)
      !outputScript = to_p2wsh $ htlc_output_script
        (hc_revocation_pubkey ctx)
        (hc_to_self_delay ctx)
        (hc_local_delayed ctx)
  in HTLCTx
       { htx_version = 2
       , htx_locktime = locktime
       , htx_input_outpoint = outpoint
       , htx_input_sequence = inputSeq
       , htx_output_value = outputValue
       , htx_output_script = outputScript
       }
{-# INLINE build_htlc_tx_common #-}

-- | Build an HTLC-timeout transaction.
--
-- * locktime: cltv_expiry
-- * sequence: 0 (or 1 with option_anchors)
-- * output: to_local style script with revocation and delayed paths
build_htlc_timeout_tx :: HTLCContext -> HTLCTx
build_htlc_timeout_tx ctx =
  let !fee = htlc_timeout_fee (hc_feerate ctx) (hc_features ctx)
      !locktime = Locktime (unCltvExpiry $ htlc_cltv_expiry $ hc_htlc ctx)
  in build_htlc_tx_common ctx locktime fee
{-# INLINE build_htlc_timeout_tx #-}

-- | Build an HTLC-success transaction.
--
-- * locktime: 0
-- * sequence: 0 (or 1 with option_anchors)
-- * output: to_local style script with revocation and delayed paths
build_htlc_success_tx :: HTLCContext -> HTLCTx
build_htlc_success_tx ctx =
  let !fee = htlc_success_fee (hc_feerate ctx) (hc_features ctx)
  in build_htlc_tx_common ctx (Locktime 0) fee
{-# INLINE build_htlc_success_tx #-}

-- closing transaction ---------------------------------------------------------

-- | Context for building closing transactions.
data ClosingContext = ClosingContext
  { clc_funding_outpoint   :: !OutPoint
  , clc_local_amount       :: !Satoshi
  , clc_remote_amount      :: !Satoshi
  , clc_local_script       :: !Script
  , clc_remote_script      :: !Script
  , clc_local_dust_limit   :: !DustLimit
  , clc_remote_dust_limit  :: !DustLimit
  , clc_fee                :: !Satoshi
  , clc_is_funder          :: !Bool
  , clc_locktime           :: !Locktime
  , clc_funding_script     :: !Script
  } deriving (Eq, Show, Generic)

-- | A closing transaction.
data ClosingTx = ClosingTx
  { cltx_version           :: {-# UNPACK #-} !Word32
  , cltx_locktime          :: !Locktime
  , cltx_input_outpoint    :: !OutPoint
  , cltx_input_sequence    :: !Sequence
  , cltx_outputs           :: ![TxOutput]
  , cltx_funding_script    :: !Script
  } deriving (Eq, Show, Generic)

-- | Build a closing transaction (option_simple_close).
--
-- * locktime: from closing_complete message
-- * sequence: 0xFFFFFFFD
-- * outputs: sorted per BIP69
build_closing_tx :: ClosingContext -> ClosingTx
build_closing_tx ctx =
  let -- Subtract fee from closer
      !(localAmt, remoteAmt) = if clc_is_funder ctx
        then
          let !deducted = if unSatoshi (clc_local_amount ctx) >=
                             unSatoshi (clc_fee ctx)
                          then Satoshi (unSatoshi (clc_local_amount ctx) -
                                        unSatoshi (clc_fee ctx))
                          else Satoshi 0
          in (deducted, clc_remote_amount ctx)
        else
          let !deducted = if unSatoshi (clc_remote_amount ctx) >=
                             unSatoshi (clc_fee ctx)
                          then Satoshi (unSatoshi (clc_remote_amount ctx) -
                                        unSatoshi (clc_fee ctx))
                          else Satoshi 0
          in (clc_local_amount ctx, deducted)

      -- Build outputs, omitting dust
      !localOutput =
        if unSatoshi localAmt >= unSatoshi (unDustLimit $ clc_local_dust_limit ctx)
        then [TxOutput localAmt (clc_local_script ctx) OutputToLocal]
        else []

      !remoteOutput =
        if unSatoshi remoteAmt >= unSatoshi (unDustLimit $ clc_remote_dust_limit ctx)
        then [TxOutput remoteAmt (clc_remote_script ctx) OutputToRemote]
        else []

      !allOutputs = localOutput ++ remoteOutput
      !sortedOutputs = sort_outputs allOutputs

  in ClosingTx
       { cltx_version = 2
       , cltx_locktime = clc_locktime ctx
       , cltx_input_outpoint = clc_funding_outpoint ctx
       , cltx_input_sequence = Sequence 0xFFFFFFFD
       , cltx_outputs = sortedOutputs
       , cltx_funding_script = clc_funding_script ctx
       }
{-# INLINE build_closing_tx #-}

-- | Build a legacy closing transaction (closing_signed).
--
-- * locktime: 0
-- * sequence: 0xFFFFFFFF
-- * outputs: sorted per BIP69
build_legacy_closing_tx :: ClosingContext -> ClosingTx
build_legacy_closing_tx ctx =
  let !result = build_closing_tx ctx
        { clc_locktime = Locktime 0 }
  in result { cltx_input_sequence = Sequence 0xFFFFFFFF }
{-# INLINE build_legacy_closing_tx #-}

-- fee calculation -------------------------------------------------------------

-- | Calculate the base commitment transaction fee.
--
-- @fee = feerate_per_kw * weight / 1000@
--
-- where @weight = base_weight + 172 * num_htlcs@
commitment_fee :: FeeratePerKw -> ChannelFeatures -> Word64 -> Satoshi
commitment_fee feerate features numHtlcs =
  let !weight = commitment_weight features numHtlcs
      !fee = (fromIntegral (unFeeratePerKw feerate) * weight) `div` 1000
  in Satoshi fee
{-# INLINE commitment_fee #-}

-- | Calculate commitment transaction weight.
--
-- @weight = base + 172 * num_htlcs@
commitment_weight :: ChannelFeatures -> Word64 -> Word64
commitment_weight features numHtlcs =
  let !base = if has_anchors features
              then commitment_weight_anchors
              else commitment_weight_no_anchors
  in base + htlc_output_weight * numHtlcs
{-# INLINE commitment_weight #-}

-- | Calculate HTLC-timeout transaction fee.
--
-- With option_anchors, fee is 0 (CPFP).
-- Otherwise, @fee = feerate_per_kw * 663 / 1000@
htlc_timeout_fee :: FeeratePerKw -> ChannelFeatures -> Satoshi
htlc_timeout_fee feerate features
  | has_anchors features = Satoshi 0
  | otherwise =
      let !weight = htlc_timeout_weight_no_anchors
          !fee = (fromIntegral (unFeeratePerKw feerate) * weight) `div` 1000
      in Satoshi fee
{-# INLINE htlc_timeout_fee #-}

-- | Calculate HTLC-success transaction fee.
--
-- With option_anchors, fee is 0 (CPFP).
-- Otherwise, @fee = feerate_per_kw * 703 / 1000@
htlc_success_fee :: FeeratePerKw -> ChannelFeatures -> Satoshi
htlc_success_fee feerate features
  | has_anchors features = Satoshi 0
  | otherwise =
      let !weight = htlc_success_weight_no_anchors
          !fee = (fromIntegral (unFeeratePerKw feerate) * weight) `div` 1000
      in Satoshi fee
{-# INLINE htlc_success_fee #-}

-- trimming --------------------------------------------------------------------

-- | Calculate the trim threshold for an HTLC.
--
-- An HTLC is trimmed if:
-- @amount < dust_limit + htlc_tx_fee@
htlc_trim_threshold
  :: DustLimit
  -> FeeratePerKw
  -> ChannelFeatures
  -> HTLCDirection
  -> Satoshi
htlc_trim_threshold dust feerate features direction =
  let !dustVal = unDustLimit dust
      !htlcFee = case direction of
        HTLCOffered  -> htlc_timeout_fee feerate features
        HTLCReceived -> htlc_success_fee feerate features
  in Satoshi (unSatoshi dustVal + unSatoshi htlcFee)
{-# INLINE htlc_trim_threshold #-}

-- | Check if an HTLC should be trimmed.
--
-- An HTLC is trimmed if its amount minus the HTLC tx fee is below
-- the dust limit.
is_trimmed :: DustLimit -> FeeratePerKw -> ChannelFeatures -> HTLC -> Bool
is_trimmed dust feerate features htlc =
  let !threshold = htlc_trim_threshold dust feerate features
                     (htlc_direction htlc)
      !amountSat = msat_to_sat (htlc_amount_msat htlc)
  in unSatoshi amountSat < unSatoshi threshold
{-# INLINE is_trimmed #-}

-- | Filter HTLCs that are trimmed.
trimmed_htlcs
  :: DustLimit
  -> FeeratePerKw
  -> ChannelFeatures
  -> [HTLC]
  -> [HTLC]
trimmed_htlcs dust feerate features =
  filter (is_trimmed dust feerate features)
{-# INLINE trimmed_htlcs #-}

-- | Filter HTLCs that are not trimmed.
untrimmed_htlcs
  :: DustLimit
  -> FeeratePerKw
  -> ChannelFeatures
  -> [HTLC]
  -> [HTLC]
untrimmed_htlcs dust feerate features =
  filter (not . is_trimmed dust feerate features)
{-# INLINE untrimmed_htlcs #-}

-- conversion to ppad-tx -------------------------------------------------------

-- | Convert a 'TxOutput' to a ppad-tx 'BT.TxOut'.
toTxOut :: TxOutput -> BT.TxOut
toTxOut o = BT.TxOut
  { BT.txout_value =
      unSatoshi (txout_value o)
  , BT.txout_script_pubkey =
      unScript (txout_script o)
  }
{-# INLINE toTxOut #-}

-- | Convert a commitment transaction to a ppad-tx 'BT.Tx'.
--
-- Returns 'Nothing' if the transaction has no outputs.
commitment_to_tx :: CommitmentTx -> Maybe BT.Tx
commitment_to_tx ctx = do
  outs <- nonEmpty (map toTxOut (ctx_outputs ctx))
  let !input = BT.TxIn
        { BT.txin_prevout = ctx_input_outpoint ctx
        , BT.txin_script_sig = BS.empty
        , BT.txin_sequence =
            unSequence (ctx_input_sequence ctx)
        }
  pure $! BT.Tx
    { BT.tx_version = ctx_version ctx
    , BT.tx_inputs = input :| []
    , BT.tx_outputs = outs
    , BT.tx_witnesses = []
    , BT.tx_locktime = unLocktime (ctx_locktime ctx)
    }

-- | Convert an HTLC transaction to a ppad-tx 'BT.Tx'.
htlc_to_tx :: HTLCTx -> BT.Tx
htlc_to_tx htx =
  let !input = BT.TxIn
        { BT.txin_prevout = htx_input_outpoint htx
        , BT.txin_script_sig = BS.empty
        , BT.txin_sequence =
            unSequence (htx_input_sequence htx)
        }
      !output = BT.TxOut
        { BT.txout_value =
            unSatoshi (htx_output_value htx)
        , BT.txout_script_pubkey =
            unScript (htx_output_script htx)
        }
  in BT.Tx
    { BT.tx_version = htx_version htx
    , BT.tx_inputs = input :| []
    , BT.tx_outputs = output :| []
    , BT.tx_witnesses = []
    , BT.tx_locktime = unLocktime (htx_locktime htx)
    }

-- | Convert a closing transaction to a ppad-tx 'BT.Tx'.
--
-- Returns 'Nothing' if the transaction has no outputs.
closing_to_tx :: ClosingTx -> Maybe BT.Tx
closing_to_tx ctx = do
  outs <- nonEmpty (map toTxOut (cltx_outputs ctx))
  let !input = BT.TxIn
        { BT.txin_prevout = cltx_input_outpoint ctx
        , BT.txin_script_sig = BS.empty
        , BT.txin_sequence =
            unSequence (cltx_input_sequence ctx)
        }
  pure $! BT.Tx
    { BT.tx_version = cltx_version ctx
    , BT.tx_inputs = input :| []
    , BT.tx_outputs = outs
    , BT.tx_witnesses = []
    , BT.tx_locktime = unLocktime (cltx_locktime ctx)
    }

-- output ordering -------------------------------------------------------------

-- | Sort outputs per BOLT #3 ordering.
--
-- Outputs are sorted by:
-- 1. Value (smallest first)
-- 2. ScriptPubKey (lexicographic)
-- 3. CLTV expiry (for HTLCs)
sort_outputs :: [TxOutput] -> [TxOutput]
sort_outputs = sortBy compareOutputs
{-# INLINE sort_outputs #-}

-- | Compare two outputs for ordering.
compareOutputs :: TxOutput -> TxOutput -> Ordering
compareOutputs o1 o2 =
  case compare (txout_value o1) (txout_value o2) of
    EQ -> case compare (unScript $ txout_script o1)
                       (unScript $ txout_script o2) of
            EQ -> compareCltvExpiry (txout_type o1) (txout_type o2)
            other -> other
    other -> other
{-# INLINE compareOutputs #-}

-- | Compare CLTV expiry for HTLC outputs.
compareCltvExpiry :: OutputType -> OutputType -> Ordering
compareCltvExpiry (OutputOfferedHTLC e1)  (OutputOfferedHTLC e2)  = compare e1 e2
compareCltvExpiry (OutputReceivedHTLC e1) (OutputReceivedHTLC e2) = compare e1 e2
compareCltvExpiry (OutputOfferedHTLC e1)  (OutputReceivedHTLC e2) = compare e1 e2
compareCltvExpiry (OutputReceivedHTLC e1) (OutputOfferedHTLC e2)  = compare e1 e2
compareCltvExpiry _ _ = EQ
{-# INLINE compareCltvExpiry #-}
