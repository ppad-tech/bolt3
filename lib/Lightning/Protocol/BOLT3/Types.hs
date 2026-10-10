{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Types
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Types for BOLT #3 transaction and script formats.

module Lightning.Protocol.BOLT3.Types (
    -- * Points and keys
    PerCommitmentPoint(..)
  , RevocationBasepoint(..)
  , PaymentBasepoint(..)
  , DelayedPaymentBasepoint(..)
  , HtlcBasepoint(..)
  , Basepoints(..)
  , RevocationPubkey(..)
  , LocalDelayedPubkey(..)
  , LocalHtlcPubkey(..)
  , RemoteHtlcPubkey(..)
  , RemotePubkey(..)
  , FundingPubkey(..)
  , Seckey(..)
  , seckey
  , un_seckey

    -- * Per-commitment secrets
  , Seed(..)
  , seed
  , un_seed
  , SecretIndex(..)
  , secret_index
  , un_secret_index
  , commitment_secret_index

    -- * Channel parameters
  , CommitmentNumber(..)
  , commitment_number
  , un_commitment_number
  , next_commitment_number
  , ToSelfDelay(..)
  , CltvExpiry(..)
  , DustLimit(..)
  , FeeratePerKw(..)
  , Locktime(..)
  , Sequence(..)
  , CommitmentFormat(..)

    -- * HTLCs
  , HTLC(..)
  , HTLCDirection(..)

    -- * Scripts
  , Script(..)

    -- * Dust thresholds
  , dust_p2pkh
  , dust_p2sh
  , dust_p2wpkh
  , dust_p2wsh
  , anchor_output_value

    -- * Internal
  , sat
  , internal_error
  ) where

import Control.DeepSeq (NFData(..))
import qualified Crypto.Curve.Secp256k1 as S
import qualified Data.ByteString as BS
import Data.Word (Word16, Word32, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1 (Point, Satoshi, MilliSatoshi, PaymentHash)
import qualified Lightning.Protocol.BOLT1 as BOLT1

-- internal ------------------------------------------------------------------

-- Abort on a state that the surrounding code rules out.
internal_error :: a
internal_error = error "ppad-bolt3: internal error, please report a bug!"
{-# NOINLINE internal_error #-}

-- A 'Satoshi' amount known to be at most 21 million BTC (a constant,
-- or a value bounded by construction).
sat :: Word64 -> Satoshi
sat w = case BOLT1.satoshi w of
  Just s  -> s
  Nothing -> internal_error
{-# INLINE sat #-}

-- points and keys ------------------------------------------------------------

-- | A per-commitment point, from which a commitment's keys are
--   derived.
newtype PerCommitmentPoint = PerCommitmentPoint Point
  deriving (Eq, Ord, Show, Generic)

instance NFData PerCommitmentPoint

-- | A @revocation_basepoint@.
newtype RevocationBasepoint = RevocationBasepoint Point
  deriving (Eq, Ord, Show, Generic)

instance NFData RevocationBasepoint

-- | A @payment_basepoint@.
newtype PaymentBasepoint = PaymentBasepoint Point
  deriving (Eq, Ord, Show, Generic)

instance NFData PaymentBasepoint

-- | A @delayed_payment_basepoint@.
newtype DelayedPaymentBasepoint = DelayedPaymentBasepoint Point
  deriving (Eq, Ord, Show, Generic)

instance NFData DelayedPaymentBasepoint

-- | An @htlc_basepoint@.
newtype HtlcBasepoint = HtlcBasepoint Point
  deriving (Eq, Ord, Show, Generic)

instance NFData HtlcBasepoint

-- | The four basepoints a party announces in @open_channel@ or
--   @accept_channel@.
data Basepoints = Basepoints
  { bp_revocation      :: !RevocationBasepoint
  , bp_payment         :: !PaymentBasepoint
  , bp_delayed_payment :: !DelayedPaymentBasepoint
  , bp_htlc            :: !HtlcBasepoint
  } deriving (Eq, Show, Generic)

instance NFData Basepoints

-- | A commitment's @revocationpubkey@.
newtype RevocationPubkey = RevocationPubkey Point
  deriving (Eq, Ord, Show, Generic)

instance NFData RevocationPubkey

-- | A commitment's @local_delayedpubkey@: the owner's key for its
--   delayed outputs.
newtype LocalDelayedPubkey = LocalDelayedPubkey Point
  deriving (Eq, Ord, Show, Generic)

instance NFData LocalDelayedPubkey

-- | A commitment's @local_htlcpubkey@: the owner's HTLC key.
newtype LocalHtlcPubkey = LocalHtlcPubkey Point
  deriving (Eq, Ord, Show, Generic)

instance NFData LocalHtlcPubkey

-- | A commitment's @remote_htlcpubkey@: the other party's HTLC key.
newtype RemoteHtlcPubkey = RemoteHtlcPubkey Point
  deriving (Eq, Ord, Show, Generic)

instance NFData RemoteHtlcPubkey

-- | A commitment's @remotepubkey@: the other party's
--   @payment_basepoint@, which its @to_remote@ output pays.
newtype RemotePubkey = RemotePubkey Point
  deriving (Eq, Ord, Show, Generic)

instance NFData RemotePubkey

-- | A @funding_pubkey@.
newtype FundingPubkey = FundingPubkey Point
  deriving (Eq, Ord, Show, Generic)

instance NFData FundingPubkey

-- | A secp256k1 secret key: 32 bytes encoding an integer in
--   [1, n), for n the group order.
--
--   This is secret material: its 'Show' instance is redacted, and it
--   has no 'Eq' instance.
newtype Seckey = Seckey BS.ByteString

instance Show Seckey where
  showsPrec d _ = showParen (d > 10) $ showString "Seckey <redacted>"

instance NFData Seckey where
  rnf (Seckey bs) = rnf bs

-- | Construct a 'Seckey' from 32 big-endian bytes. Fails unless the
--   bytes encode an integer in [1, n).
--
--   >>> fmap (BS.length . un_seckey) (seckey (BS.replicate 32 0x01))
--   Just 32
--   >>> seckey (BS.replicate 32 0x00)
--   Nothing
--   >>> seckey (BS.replicate 32 0xff)
--   Nothing
seckey :: BS.ByteString -> Maybe Seckey
seckey bs = do
  w <- S.parse_int256 bs
  if   S.ge w
  then Just (Seckey bs)
  else Nothing

-- | The bytes of a 'Seckey'.
un_seckey :: Seckey -> BS.ByteString
un_seckey (Seckey bs) = bs
{-# INLINE un_seckey #-}

-- per-commitment secrets -----------------------------------------------------

-- | The 32-byte seed from which a party generates its per-commitment
--   secrets.
--
--   This is secret material: its 'Show' instance is redacted, and it
--   has no 'Eq' instance.
newtype Seed = Seed BS.ByteString

instance Show Seed where
  showsPrec d _ = showParen (d > 10) $ showString "Seed <redacted>"

instance NFData Seed where
  rnf (Seed bs) = rnf bs

-- | Construct a 'Seed' from exactly 32 bytes.
--
--   >>> fmap (BS.length . un_seed) (seed (BS.replicate 32 0xff))
--   Just 32
--   >>> seed (BS.replicate 31 0xff)
--   Nothing
seed :: BS.ByteString -> Maybe Seed
seed bs
  | BS.length bs == 32 = Just (Seed bs)
  | otherwise          = Nothing
{-# INLINE seed #-}

-- | The bytes of a 'Seed'.
un_seed :: Seed -> BS.ByteString
un_seed (Seed bs) = bs
{-# INLINE un_seed #-}

-- | The 48-bit index /I/ of a per-commitment secret. Secrets are
--   used from index 2^48 - 1 downwards.
newtype SecretIndex = SecretIndex Word64
  deriving (Eq, Ord, Show, Generic)

instance NFData SecretIndex

-- | Construct a 'SecretIndex'. Fails at or above 2^48.
--
--   >>> secret_index 281474976710655
--   Just (SecretIndex 281474976710655)
--   >>> secret_index 281474976710656
--   Nothing
secret_index :: Word64 -> Maybe SecretIndex
secret_index w
  | w <= 0xFFFFFFFFFFFF = Just (SecretIndex w)
  | otherwise           = Nothing
{-# INLINE secret_index #-}

-- | The value of a 'SecretIndex'.
un_secret_index :: SecretIndex -> Word64
un_secret_index (SecretIndex w) = w
{-# INLINE un_secret_index #-}

-- | The index of the per-commitment secret for a commitment number:
--   2^48 - 1 - n.
--
--   >>> fmap commitment_secret_index (commitment_number 0)
--   Just (SecretIndex 281474976710655)
commitment_secret_index :: CommitmentNumber -> SecretIndex
commitment_secret_index (CommitmentNumber n) = SecretIndex (0xFFFFFFFFFFFF - n)
{-# INLINE commitment_secret_index #-}

-- channel parameters ---------------------------------------------------------

-- | A 48-bit commitment number.
newtype CommitmentNumber = CommitmentNumber Word64
  deriving (Eq, Ord, Show, Generic)

instance NFData CommitmentNumber

-- | Construct a 'CommitmentNumber'. Fails at or above 2^48.
--
--   >>> commitment_number 42
--   Just (CommitmentNumber 42)
--   >>> commitment_number 281474976710656
--   Nothing
commitment_number :: Word64 -> Maybe CommitmentNumber
commitment_number n
  | n <= 0xFFFFFFFFFFFF = Just (CommitmentNumber n)
  | otherwise           = Nothing
{-# INLINE commitment_number #-}

-- | The value of a 'CommitmentNumber'.
un_commitment_number :: CommitmentNumber -> Word64
un_commitment_number (CommitmentNumber n) = n
{-# INLINE un_commitment_number #-}

-- | The next commitment number. Fails past 2^48 - 1.
--
--   >>> commitment_number 0 >>= next_commitment_number
--   Just (CommitmentNumber 1)
--   >>> commitment_number 281474976710655 >>= next_commitment_number
--   Nothing
next_commitment_number :: CommitmentNumber -> Maybe CommitmentNumber
next_commitment_number (CommitmentNumber n)
  | n < 0xFFFFFFFFFFFF = Just (CommitmentNumber (n + 1))
  | otherwise          = Nothing
{-# INLINE next_commitment_number #-}

-- | The CSV delay (@to_self_delay@) on a commitment owner's outputs.
newtype ToSelfDelay = ToSelfDelay Word16
  deriving (Eq, Ord, Show, Generic)

instance NFData ToSelfDelay

-- | An HTLC's absolute CLTV expiry.
newtype CltvExpiry = CltvExpiry Word32
  deriving (Eq, Ord, Show, Generic)

instance NFData CltvExpiry

-- | A @dust_limit_satoshis@ threshold.
newtype DustLimit = DustLimit Satoshi
  deriving (Eq, Ord, Show, Generic)

instance NFData DustLimit

-- | A fee rate, in satoshis per 1000 weight units.
newtype FeeratePerKw = FeeratePerKw Word32
  deriving (Eq, Ord, Show, Generic)

instance NFData FeeratePerKw

-- | A transaction locktime.
newtype Locktime = Locktime Word32
  deriving (Eq, Ord, Show, Generic)

instance NFData Locktime

-- | A transaction input's sequence number.
newtype Sequence = Sequence Word32
  deriving (Eq, Ord, Show, Generic)

instance NFData Sequence

-- | The commitment format a channel uses, as fixed by its
--   @channel_type@.
data CommitmentFormat
  = StaticRemotekey
    -- ^ @option_static_remotekey@ without anchors: @to_remote@ is
    --   P2WPKH and HTLC transactions pay their own fees.
  | Anchors
    -- ^ @option_anchors@: two anchor outputs, a CSV-locked
    --   @to_remote@, and zero-fee HTLC transactions.
  deriving (Eq, Ord, Show, Generic)

instance NFData CommitmentFormat

-- HTLCs ----------------------------------------------------------------------

-- | The direction of an HTLC, from the commitment owner's point of
--   view.
data HTLCDirection
  = HTLCOffered   -- ^ offered by the owner
  | HTLCReceived  -- ^ received by the owner
  deriving (Eq, Ord, Show, Generic)

instance NFData HTLCDirection

-- | An HTLC committed to a commitment transaction.
data HTLC = HTLC
  { htlc_direction    :: !HTLCDirection
  , htlc_amount_msat  :: {-# UNPACK #-} !MilliSatoshi
  , htlc_payment_hash :: !PaymentHash
  , htlc_cltv_expiry  :: {-# UNPACK #-} !CltvExpiry
  } deriving (Eq, Show, Generic)

instance NFData HTLC

-- scripts --------------------------------------------------------------------

-- | A serialized Bitcoin script.
newtype Script = Script BS.ByteString
  deriving (Eq, Ord, Show, Generic)

instance NFData Script

-- dust thresholds ------------------------------------------------------------

-- | Bitcoin Core's P2PKH dust threshold (546 satoshis).
dust_p2pkh :: Satoshi
dust_p2pkh = sat 546

-- | Bitcoin Core's P2SH dust threshold (540 satoshis).
dust_p2sh :: Satoshi
dust_p2sh = sat 540

-- | Bitcoin Core's P2WPKH dust threshold (294 satoshis).
dust_p2wpkh :: Satoshi
dust_p2wpkh = sat 294

-- | Bitcoin Core's P2WSH dust threshold (330 satoshis).
dust_p2wsh :: Satoshi
dust_p2wsh = sat 330

-- | The value of each anchor output (330 satoshis).
anchor_output_value :: Satoshi
anchor_output_value = sat 330
