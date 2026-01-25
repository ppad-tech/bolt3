{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Validate
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Stateless validation for BOLT #3 transactions.
--
-- Provides validation for:
--
-- * Commitment transaction structure and outputs
-- * HTLC transaction structure
-- * Closing transaction structure
-- * Output ordering per BIP69+CLTV
-- * Dust limit compliance

module Lightning.Protocol.BOLT3.Validate (
    -- * Validation errors
    ValidationError(..)

    -- * Commitment transaction validation
  , validate_commitment_tx
  , validate_commitment_locktime
  , validate_commitment_sequence

    -- * HTLC transaction validation
  , validate_htlc_tx
  , validate_htlc_timeout_tx
  , validate_htlc_success_tx

    -- * Closing transaction validation
  , validate_closing_tx
  , validate_legacy_closing_tx

    -- * Output validation
  , validate_output_ordering
  , validate_dust_limits
  , validate_anchor_outputs

    -- * Fee validation
  , validate_commitment_fee
  , validate_htlc_fee
  ) where

import Data.Bits ((.&.), shiftR)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT3.Types
import Lightning.Protocol.BOLT3.Tx

-- validation errors -----------------------------------------------------------

-- | Errors that can occur during validation.
data ValidationError
  = InvalidVersion {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
    -- ^ Expected version, actual version
  | InvalidLocktime {-# UNPACK #-} !Word32
    -- ^ Invalid locktime format
  | InvalidSequence {-# UNPACK #-} !Word32
    -- ^ Invalid sequence format
  | InvalidOutputOrdering
    -- ^ Outputs not in BIP69+CLTV order
  | DustLimitViolation {-# UNPACK #-} !Int !Satoshi !Satoshi
    -- ^ Output index, actual value, dust limit
  | MissingAnchorOutput
    -- ^ Expected anchor output not present
  | InvalidAnchorValue {-# UNPACK #-} !Satoshi
    -- ^ Anchor value not 330 satoshis
  | InvalidFee {-# UNPACK #-} !Satoshi {-# UNPACK #-} !Satoshi
    -- ^ Expected fee, actual fee
  | InvalidHTLCLocktime {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
    -- ^ Expected locktime, actual locktime
  | InvalidHTLCSequence {-# UNPACK #-} !Word32 {-# UNPACK #-} !Word32
    -- ^ Expected sequence, actual sequence
  | NoOutputs
    -- ^ Transaction has no outputs
  | TooManyOutputs {-# UNPACK #-} !Int
    -- ^ More outputs than expected
  deriving (Eq, Show, Generic)

-- commitment transaction validation -------------------------------------------

-- | Validate a commitment transaction.
--
-- Checks:
--
-- * Version is 2
-- * Locktime format (upper 8 bits = 0x20)
-- * Sequence format (upper 8 bits = 0x80)
-- * Output ordering per BIP69+CLTV
-- * Dust limit compliance
-- * Anchor outputs if option_anchors
validate_commitment_tx
  :: DustLimit
  -> ChannelFeatures
  -> CommitmentTx
  -> Either ValidationError ()
validate_commitment_tx dust features tx = do
  -- Version must be 2
  validateVersion 2 (ctx_version tx)
  -- Locktime format
  validate_commitment_locktime (ctx_locktime tx)
  -- Sequence format
  validate_commitment_sequence (ctx_input_sequence tx)
  -- Output ordering
  validate_output_ordering (ctx_outputs tx)
  -- Dust limits
  validate_dust_limits dust (ctx_outputs tx)
  -- Anchors if applicable
  if has_anchors features
    then validate_anchor_outputs (ctx_outputs tx)
    else pure ()
{-# INLINE validate_commitment_tx #-}

-- | Validate commitment transaction locktime format.
--
-- Upper 8 bits must be 0x20.
validate_commitment_locktime :: Locktime -> Either ValidationError ()
validate_commitment_locktime (Locktime lt) =
  let !upper = (lt `shiftR` 24) .&. 0xFF
  in if upper == 0x20
     then Right ()
     else Left (InvalidLocktime lt)
{-# INLINE validate_commitment_locktime #-}

-- | Validate commitment transaction sequence format.
--
-- Upper 8 bits must be 0x80.
validate_commitment_sequence :: Sequence -> Either ValidationError ()
validate_commitment_sequence (Sequence sq) =
  let !upper = (sq `shiftR` 24) .&. 0xFF
  in if upper == 0x80
     then Right ()
     else Left (InvalidSequence sq)
{-# INLINE validate_commitment_sequence #-}

-- HTLC transaction validation -------------------------------------------------

-- | Validate an HTLC transaction (timeout or success).
--
-- Checks:
--
-- * Version is 2
-- * Single output
validate_htlc_tx :: HTLCTx -> Either ValidationError ()
validate_htlc_tx tx = do
  validateVersion 2 (htx_version tx)
  pure ()
{-# INLINE validate_htlc_tx #-}

-- | Validate an HTLC-timeout transaction.
--
-- Checks:
--
-- * Base HTLC validation
-- * Locktime equals HTLC cltv_expiry
-- * Sequence is 0 (or 1 with option_anchors)
validate_htlc_timeout_tx
  :: ChannelFeatures
  -> CltvExpiry
  -> HTLCTx
  -> Either ValidationError ()
validate_htlc_timeout_tx features expiry tx = do
  validate_htlc_tx tx
  -- Locktime must be cltv_expiry
  let !expectedLt = unCltvExpiry expiry
      !actualLt = unLocktime (htx_locktime tx)
  if expectedLt == actualLt
    then pure ()
    else Left (InvalidHTLCLocktime expectedLt actualLt)
  -- Sequence
  let !expectedSeq = if has_anchors features then 1 else 0
      !actualSeq = unSequence (htx_input_sequence tx)
  if expectedSeq == actualSeq
    then pure ()
    else Left (InvalidHTLCSequence expectedSeq actualSeq)
{-# INLINE validate_htlc_timeout_tx #-}

-- | Validate an HTLC-success transaction.
--
-- Checks:
--
-- * Base HTLC validation
-- * Locktime is 0
-- * Sequence is 0 (or 1 with option_anchors)
validate_htlc_success_tx
  :: ChannelFeatures
  -> HTLCTx
  -> Either ValidationError ()
validate_htlc_success_tx features tx = do
  validate_htlc_tx tx
  -- Locktime must be 0
  let !actualLt = unLocktime (htx_locktime tx)
  if actualLt == 0
    then pure ()
    else Left (InvalidHTLCLocktime 0 actualLt)
  -- Sequence
  let !expectedSeq = if has_anchors features then 1 else 0
      !actualSeq = unSequence (htx_input_sequence tx)
  if expectedSeq == actualSeq
    then pure ()
    else Left (InvalidHTLCSequence expectedSeq actualSeq)
{-# INLINE validate_htlc_success_tx #-}

-- closing transaction validation ----------------------------------------------

-- | Validate a closing transaction (option_simple_close).
--
-- Checks:
--
-- * Version is 2
-- * Sequence is 0xFFFFFFFD
-- * At least one output
-- * Output ordering per BIP69
validate_closing_tx :: ClosingTx -> Either ValidationError ()
validate_closing_tx tx = do
  validateVersion 2 (cltx_version tx)
  let !actualSeq = unSequence (cltx_input_sequence tx)
  if actualSeq == 0xFFFFFFFD
    then pure ()
    else Left (InvalidSequence actualSeq)
  validateOutputCount (cltx_outputs tx)
  validate_output_ordering (cltx_outputs tx)
{-# INLINE validate_closing_tx #-}

-- | Validate a legacy closing transaction (closing_signed).
--
-- Checks:
--
-- * Version is 2
-- * Locktime is 0
-- * Sequence is 0xFFFFFFFF
-- * At least one output
-- * Output ordering per BIP69
validate_legacy_closing_tx :: ClosingTx -> Either ValidationError ()
validate_legacy_closing_tx tx = do
  validateVersion 2 (cltx_version tx)
  let !actualLt = unLocktime (cltx_locktime tx)
  if actualLt == 0
    then pure ()
    else Left (InvalidLocktime actualLt)
  let !actualSeq = unSequence (cltx_input_sequence tx)
  if actualSeq == 0xFFFFFFFF
    then pure ()
    else Left (InvalidSequence actualSeq)
  validateOutputCount (cltx_outputs tx)
  validate_output_ordering (cltx_outputs tx)
{-# INLINE validate_legacy_closing_tx #-}

-- output validation -----------------------------------------------------------

-- | Validate output ordering per BIP69+CLTV.
--
-- Outputs must be sorted by:
-- 1. Value (smallest first)
-- 2. ScriptPubKey (lexicographic)
-- 3. CLTV expiry (for HTLC outputs)
validate_output_ordering :: [TxOutput] -> Either ValidationError ()
validate_output_ordering outputs =
  let !sorted = sort_outputs outputs
  in if outputs == sorted
     then Right ()
     else Left InvalidOutputOrdering
{-# INLINE validate_output_ordering #-}

-- | Validate that no output violates dust limits.
validate_dust_limits
  :: DustLimit
  -> [TxOutput]
  -> Either ValidationError ()
validate_dust_limits dust = go 0 where
  !limit = unDustLimit dust
  go !_ [] = Right ()
  go !idx (out:rest) =
    let !val = txout_value out
    in case txout_type out of
         -- Anchors have fixed value, don't check against dust limit
         OutputLocalAnchor -> go (idx + 1) rest
         OutputRemoteAnchor -> go (idx + 1) rest
         -- All other outputs must be above dust
         _ -> if unSatoshi val >= unSatoshi limit
              then go (idx + 1) rest
              else Left (DustLimitViolation idx val limit)
{-# INLINE validate_dust_limits #-}

-- | Validate anchor outputs are present and correctly valued.
validate_anchor_outputs :: [TxOutput] -> Either ValidationError ()
validate_anchor_outputs outputs =
  let !anchors = filter isAnchor outputs
  in if null anchors
     then Left MissingAnchorOutput
     else validateAnchorValues anchors
  where
    isAnchor out = case txout_type out of
      OutputLocalAnchor  -> True
      OutputRemoteAnchor -> True
      _ -> False

    validateAnchorValues [] = Right ()
    validateAnchorValues (a:as) =
      let !val = txout_value a
      in if val == anchor_output_value
         then validateAnchorValues as
         else Left (InvalidAnchorValue val)
{-# INLINE validate_anchor_outputs #-}

-- fee validation --------------------------------------------------------------

-- | Validate commitment transaction fee.
--
-- Checks that the fee matches the expected calculation.
validate_commitment_fee
  :: FeeratePerKw
  -> ChannelFeatures
  -> Word64           -- ^ Number of untrimmed HTLCs
  -> Satoshi          -- ^ Actual fee
  -> Either ValidationError ()
validate_commitment_fee feerate features numHtlcs actualFee =
  let !expectedFee = commitment_fee feerate features numHtlcs
  in if actualFee == expectedFee
     then Right ()
     else Left (InvalidFee expectedFee actualFee)
{-# INLINE validate_commitment_fee #-}

-- | Validate HTLC transaction fee.
validate_htlc_fee
  :: FeeratePerKw
  -> ChannelFeatures
  -> HTLCDirection
  -> Satoshi          -- ^ Actual fee
  -> Either ValidationError ()
validate_htlc_fee feerate features direction actualFee =
  let !expectedFee = case direction of
        HTLCOffered  -> htlc_timeout_fee feerate features
        HTLCReceived -> htlc_success_fee feerate features
  in if actualFee == expectedFee
     then Right ()
     else Left (InvalidFee expectedFee actualFee)
{-# INLINE validate_htlc_fee #-}

-- helpers ---------------------------------------------------------------------

-- | Validate transaction version.
validateVersion :: Word32 -> Word32 -> Either ValidationError ()
validateVersion expected actual =
  if expected == actual
  then Right ()
  else Left (InvalidVersion expected actual)
{-# INLINE validateVersion #-}

-- | Validate that transaction has at least one output.
validateOutputCount :: [TxOutput] -> Either ValidationError ()
validateOutputCount [] = Left NoOutputs
validateOutputCount _  = Right ()
{-# INLINE validateOutputCount #-}
