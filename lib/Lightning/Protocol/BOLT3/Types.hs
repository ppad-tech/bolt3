{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Types
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Core types for BOLT #3 transaction and script formats.

module Lightning.Protocol.BOLT3.Types (
    -- * Monetary amounts (re-exported from BOLT1)
    Satoshi(..)
  , MilliSatoshi(..)
  , msatToSat
  , satToMsat

    -- * Keys and points
  , Pubkey(..)
  , pubkey
  , Seckey(..)
  , seckey
  , Point(..)
  , point

    -- * Hashes (re-exported from BOLT1)
  , PaymentHash(..)
  , paymentHash
  , PaymentPreimage(..)
  , paymentPreimage

    -- * Transaction primitives
  , TxId(..)
  , mkTxId
  , OutPoint(..)
  , Sequence(..)
  , Locktime(..)

    -- * Channel parameters
  , CommitmentNumber(..)
  , commitment_number
  , ToSelfDelay(..)
  , CltvExpiry(..)
  , DustLimit(..)
  , FeeratePerKw(..)

    -- * HTLC types
  , HTLC(..)
  , HTLCDirection(..)

    -- * Basepoints
  , Basepoints(..)
  , PerCommitmentPoint(..)
  , PerCommitmentSecret(..)
  , perCommitmentSecret
  , RevocationBasepoint(..)
  , PaymentBasepoint(..)
  , DelayedPaymentBasepoint(..)
  , HtlcBasepoint(..)

    -- * Derived keys
  , LocalPubkey(..)
  , RemotePubkey(..)
  , LocalDelayedPubkey(..)
  , RemoteDelayedPubkey(..)
  , LocalHtlcPubkey(..)
  , RemoteHtlcPubkey(..)
  , RevocationPubkey(..)
  , FundingPubkey(..)

    -- * Script
  , Script(..)

    -- * Witness (re-exported from ppad-tx)
  , Witness(..)

    -- * Channel options
  , ChannelFeatures(..)
  , has_anchors

    -- * Transaction weights (constants)
  , commitment_weight_no_anchors
  , commitment_weight_anchors
  , htlc_timeout_weight_no_anchors
  , htlc_timeout_weight_anchors
  , htlc_success_weight_no_anchors
  , htlc_success_weight_anchors
  , htlc_output_weight

    -- * Dust thresholds (constants)
  , dust_p2pkh
  , dust_p2sh
  , dust_p2wpkh
  , dust_p2wsh
  , anchor_output_value
  ) where

import Bitcoin.Prim.Tx (TxId(..), mkTxId, OutPoint(..), Witness(..))
import Data.Word (Word16, Word32, Word64)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1.Prim
  ( Satoshi(..), MilliSatoshi(..)
  , satToMsat, msatToSat
  , Point(..), point
  , PaymentHash(..), paymentHash
  , PaymentPreimage(..), paymentPreimage
  , PerCommitmentSecret(..), perCommitmentSecret
  )

-- keys and points -------------------------------------------------------------

-- | Compressed public key (33 bytes).
newtype Pubkey = Pubkey { unPubkey :: BS.ByteString }
  deriving (Eq, Ord, Show, Generic)

-- | Parse a 33-byte compressed public key.
--
-- Returns Nothing if the input is not exactly 33 bytes.
--
-- >>> pubkey (BS.replicate 33 0x02)
-- Just (Pubkey ...)
-- >>> pubkey (BS.replicate 32 0x02)
-- Nothing
pubkey :: BS.ByteString -> Maybe Pubkey
pubkey bs
  | BS.length bs == 33 = Just (Pubkey bs)
  | otherwise = Nothing
{-# INLINE pubkey #-}

-- | Secret key (32 bytes).
newtype Seckey = Seckey { unSeckey :: BS.ByteString }
  deriving (Eq, Generic)

-- Don't show secret keys
instance Show Seckey where
  show _ = "Seckey <redacted>"

-- | Parse a 32-byte secret key.
--
-- Returns Nothing if the input is not exactly 32 bytes.
seckey :: BS.ByteString -> Maybe Seckey
seckey bs
  | BS.length bs == 32 = Just (Seckey bs)
  | otherwise = Nothing
{-# INLINE seckey #-}

-- transaction primitives ------------------------------------------------------

-- | Transaction input sequence number.
newtype Sequence = Sequence { unSequence :: Word32 }
  deriving (Eq, Ord, Show, Generic, Num)

-- | Transaction locktime.
newtype Locktime = Locktime { unLocktime :: Word32 }
  deriving (Eq, Ord, Show, Generic, Num)

-- channel parameters ----------------------------------------------------------

-- | 48-bit commitment number.
newtype CommitmentNumber = CommitmentNumber
  { unCommitmentNumber :: Word64 }
  deriving (Eq, Ord, Show, Generic, Num)

-- | Parse a 48-bit commitment number.
--
-- Returns Nothing if the value exceeds 2^48 - 1.
commitment_number :: Word64 -> Maybe CommitmentNumber
commitment_number n
  | n <= 281474976710655 = Just (CommitmentNumber n)
  | otherwise = Nothing
{-# INLINE commitment_number #-}

-- | CSV delay for to_local outputs.
newtype ToSelfDelay = ToSelfDelay { unToSelfDelay :: Word16 }
  deriving (Eq, Ord, Show, Generic, Num)

-- | CLTV expiry for HTLCs.
newtype CltvExpiry = CltvExpiry { unCltvExpiry :: Word32 }
  deriving (Eq, Ord, Show, Generic, Num)

-- | Dust limit threshold.
newtype DustLimit = DustLimit { unDustLimit :: Satoshi }
  deriving (Eq, Ord, Show, Generic)

-- | Fee rate in satoshis per 1000 weight units.
newtype FeeratePerKw = FeeratePerKw { unFeeratePerKw :: Word32 }
  deriving (Eq, Ord, Show, Generic, Num)

-- HTLC types ------------------------------------------------------------------

-- | Direction of an HTLC from the commitment tx owner's perspective.
data HTLCDirection
  = HTLCOffered   -- ^ We offered this HTLC (outgoing)
  | HTLCReceived  -- ^ We received this HTLC (incoming)
  deriving (Eq, Ord, Show, Generic)

-- | HTLC output details.
data HTLC = HTLC
  { htlc_direction    :: !HTLCDirection
  , htlc_amount_msat  :: {-# UNPACK #-} !MilliSatoshi
  , htlc_payment_hash :: {-# UNPACK #-} !PaymentHash
  , htlc_cltv_expiry  :: {-# UNPACK #-} !CltvExpiry
  } deriving (Eq, Show, Generic)

-- basepoints ------------------------------------------------------------------

-- | Per-commitment point (used to derive keys).
newtype PerCommitmentPoint = PerCommitmentPoint
  { unPerCommitmentPoint :: Point }
  deriving (Eq, Ord, Show, Generic)

-- | Revocation basepoint.
newtype RevocationBasepoint = RevocationBasepoint
  { unRevocationBasepoint :: Point }
  deriving (Eq, Ord, Show, Generic)

-- | Payment basepoint.
newtype PaymentBasepoint = PaymentBasepoint
  { unPaymentBasepoint :: Point }
  deriving (Eq, Ord, Show, Generic)

-- | Delayed payment basepoint.
newtype DelayedPaymentBasepoint = DelayedPaymentBasepoint
  { unDelayedPaymentBasepoint :: Point }
  deriving (Eq, Ord, Show, Generic)

-- | HTLC basepoint.
newtype HtlcBasepoint = HtlcBasepoint
  { unHtlcBasepoint :: Point }
  deriving (Eq, Ord, Show, Generic)

-- | Collection of all basepoints for one party.
data Basepoints = Basepoints
  { bp_revocation      :: !RevocationBasepoint
  , bp_payment         :: !PaymentBasepoint
  , bp_delayed_payment :: !DelayedPaymentBasepoint
  , bp_htlc            :: !HtlcBasepoint
  } deriving (Eq, Show, Generic)

-- derived keys ----------------------------------------------------------------

-- | Local pubkey.
newtype LocalPubkey = LocalPubkey { unLocalPubkey :: Pubkey }
  deriving (Eq, Ord, Show, Generic)

-- | Remote pubkey.
newtype RemotePubkey = RemotePubkey
  { unRemotePubkey :: Pubkey }
  deriving (Eq, Ord, Show, Generic)

-- | Local delayed pubkey.
newtype LocalDelayedPubkey = LocalDelayedPubkey
  { unLocalDelayedPubkey :: Pubkey }
  deriving (Eq, Ord, Show, Generic)

-- | Remote delayed pubkey.
newtype RemoteDelayedPubkey = RemoteDelayedPubkey
  { unRemoteDelayedPubkey :: Pubkey }
  deriving (Eq, Ord, Show, Generic)

-- | Local HTLC pubkey.
newtype LocalHtlcPubkey = LocalHtlcPubkey
  { unLocalHtlcPubkey :: Pubkey }
  deriving (Eq, Ord, Show, Generic)

-- | Remote HTLC pubkey.
newtype RemoteHtlcPubkey = RemoteHtlcPubkey
  { unRemoteHtlcPubkey :: Pubkey }
  deriving (Eq, Ord, Show, Generic)

-- | Revocation pubkey.
newtype RevocationPubkey = RevocationPubkey
  { unRevocationPubkey :: Pubkey }
  deriving (Eq, Ord, Show, Generic)

-- | Funding pubkey (used in 2-of-2 multisig).
newtype FundingPubkey = FundingPubkey
  { unFundingPubkey :: Pubkey }
  deriving (Eq, Ord, Show, Generic)

-- script ----------------------------------------------------------------------

-- | Bitcoin script (serialized).
newtype Script = Script { unScript :: BS.ByteString }
  deriving (Eq, Ord, Show, Generic)

-- channel options -------------------------------------------------------------

-- | Channel feature flags relevant to BOLT #3.
data ChannelFeatures = ChannelFeatures
  { cf_option_anchors :: !Bool
  } deriving (Eq, Show, Generic)

-- | Check if option_anchors is enabled.
has_anchors :: ChannelFeatures -> Bool
has_anchors = cf_option_anchors
{-# INLINE has_anchors #-}

-- transaction weights (constants from spec) -----------------------------------

-- | Base commitment tx weight without option_anchors.
commitment_weight_no_anchors :: Word64
commitment_weight_no_anchors = 724

-- | Base commitment tx weight with option_anchors.
commitment_weight_anchors :: Word64
commitment_weight_anchors = 1124

-- | HTLC-timeout tx weight without option_anchors.
htlc_timeout_weight_no_anchors :: Word64
htlc_timeout_weight_no_anchors = 663

-- | HTLC-timeout tx weight with option_anchors.
htlc_timeout_weight_anchors :: Word64
htlc_timeout_weight_anchors = 666

-- | HTLC-success tx weight without option_anchors.
htlc_success_weight_no_anchors :: Word64
htlc_success_weight_no_anchors = 703

-- | HTLC-success tx weight with option_anchors.
htlc_success_weight_anchors :: Word64
htlc_success_weight_anchors = 706

-- | Weight added per HTLC output in commitment tx.
htlc_output_weight :: Word64
htlc_output_weight = 172

-- dust thresholds (constants from Bitcoin Core) -------------------------------

-- | P2PKH dust threshold (546 satoshis).
dust_p2pkh :: Satoshi
dust_p2pkh = Satoshi 546

-- | P2SH dust threshold (540 satoshis).
dust_p2sh :: Satoshi
dust_p2sh = Satoshi 540

-- | P2WPKH dust threshold (294 satoshis).
dust_p2wpkh :: Satoshi
dust_p2wpkh = Satoshi 294

-- | P2WSH dust threshold (330 satoshis).
dust_p2wsh :: Satoshi
dust_p2wsh = Satoshi 330

-- | Fixed anchor output value (330 satoshis).
anchor_output_value :: Satoshi
anchor_output_value = Satoshi 330
