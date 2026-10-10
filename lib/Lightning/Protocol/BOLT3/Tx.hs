{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Tx
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Commitment, HTLC and closing transactions, per BOLT #3.

module Lightning.Protocol.BOLT3.Tx (
    -- * Commitment transactions
    CommitmentContext(..)
  , CommitmentTx(..)
  , CommitmentOutput(..)
  , OutputType(..)
  , build_commitment_tx

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

    -- * Fees
  , commitment_fee
  , htlc_timeout_fee
  , htlc_success_fee

    -- * Trimming
  , htlc_trim_threshold
  , is_trimmed

    -- * Serialization
  , commitment_to_tx
  , htlc_to_tx
  , closing_to_tx
  , encode_commitment_tx
  , encode_htlc_tx
  , encode_closing_tx
  ) where

import Bitcoin.Prim.Tx (OutPoint(..), TxId)
import qualified Bitcoin.Prim.Tx as BT
import Control.DeepSeq (NFData)
import Data.Bits ((.&.), (.|.), shiftR)
import qualified Data.ByteString as BS
import Data.List (sortBy)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1 (Satoshi, MilliSatoshi)
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT3.Keys
import Lightning.Protocol.BOLT3.Scripts
import Lightning.Protocol.BOLT3.Types

-- commitment transactions ----------------------------------------------------

-- | What a commitment transaction output pays.
data OutputType
  = OutputToLocal
  | OutputToRemote
  | OutputLocalAnchor
  | OutputRemoteAnchor
  | OutputHTLC !HTLC
    -- ^ an offered or received HTLC (per its 'htlc_direction')
  deriving (Eq, Show, Generic)

instance NFData OutputType

-- | A commitment transaction output.
data CommitmentOutput = CommitmentOutput
  { co_value  :: {-# UNPACK #-} !Satoshi
  , co_script :: !Script     -- ^ scriptPubKey
  , co_type   :: !OutputType
  } deriving (Eq, Show, Generic)

instance NFData CommitmentOutput

-- | The parameters of a commitment transaction, from the point of view
--   of its owner (the "local" party, whose outputs are delayed).
data CommitmentContext = CommitmentContext
  { cc_funding_outpoint  :: {-# UNPACK #-} !OutPoint
  , cc_commitment_number :: {-# UNPACK #-} !CommitmentNumber
  , cc_local_payment_bp  :: !PaymentBasepoint
    -- ^ the owner's @payment_basepoint@
  , cc_remote_payment_bp :: !PaymentBasepoint
    -- ^ the other party's @payment_basepoint@
  , cc_to_self_delay     :: {-# UNPACK #-} !ToSelfDelay
    -- ^ the delay on the owner's outputs (set by the other party)
  , cc_dust_limit        :: {-# UNPACK #-} !DustLimit
    -- ^ the owner's @dust_limit_satoshis@
  , cc_feerate           :: {-# UNPACK #-} !FeeratePerKw
  , cc_format            :: !CommitmentFormat
  , cc_is_funder         :: !Bool
    -- ^ whether the owner opened the channel, and so pays the fee
  , cc_to_local_msat     :: {-# UNPACK #-} !MilliSatoshi
  , cc_to_remote_msat    :: {-# UNPACK #-} !MilliSatoshi
  , cc_htlcs             :: ![HTLC]
  , cc_keys              :: !CommitmentKeys
  } deriving (Eq, Show, Generic)

instance NFData CommitmentContext

-- | An unsigned commitment transaction.
data CommitmentTx = CommitmentTx
  { ctx_version        :: {-# UNPACK #-} !Word32
  , ctx_locktime       :: {-# UNPACK #-} !Locktime
  , ctx_input_outpoint :: {-# UNPACK #-} !OutPoint
  , ctx_input_sequence :: {-# UNPACK #-} !Sequence
  , ctx_outputs        :: !(NonEmpty CommitmentOutput)
    -- ^ in BIP69+CLTV order
  , ctx_funding_script :: !Script
    -- ^ the funding output's witness script, which signatures commit
    --   to
  } deriving (Eq, Show, Generic)

instance NFData CommitmentTx

-- | Build a commitment transaction, per BOLT #3's "Commitment
--   Transaction Construction":
--
--   * the locktime and input sequence encode the obscured commitment
--     number (the payment basepoints are ordered by 'cc_is_funder');
--   * HTLCs whose amount, less the HTLC transaction fee, is below the
--     dust limit are trimmed;
--   * the base fee (and under 'Anchors', both anchors) are taken from
--     the funder's output, which is left at zero if it can't cover
--     them;
--   * @to_local@ and @to_remote@ outputs below the dust limit are
--     omitted, and under 'Anchors' each party's anchor is added if its
--     output, or any HTLC output, exists;
--   * outputs are sorted by value, scriptPubKey and CLTV expiry.
--
--   Fails if the transaction would have no outputs.
build_commitment_tx :: CommitmentContext -> Maybe CommitmentTx
build_commitment_tx ctx = do
    outputs <- NE.nonEmpty . sort_outputs $
      to_local <> to_remote <> local_anchor <> remote_anchor <> htlcs
    pure CommitmentTx
      { ctx_version        = 2
      , ctx_locktime       =
          Locktime (0x20000000 .|. (fromIntegral obscured .&. 0xFFFFFF))
      , ctx_input_outpoint = cc_funding_outpoint ctx
      , ctx_input_sequence =
          Sequence
            (0x80000000 .|. (fromIntegral (obscured `shiftR` 24) .&. 0xFFFFFF))
      , ctx_outputs        = outputs
      , ctx_funding_script =
          funding_script (ck_local_funding keys) (ck_remote_funding keys)
      }
  where
    !keys = cc_keys ctx
    !fmt  = cc_format ctx
    !rate = cc_feerate ctx
    !dust = cc_dust_limit ctx
    DustLimit dust_sat = dust

    (!opener, !accepter)
      | cc_is_funder ctx = (cc_local_payment_bp ctx, cc_remote_payment_bp ctx)
      | otherwise        = (cc_remote_payment_bp ctx, cc_local_payment_bp ctx)
    !obscured =
      obscured_commitment_number opener accepter (cc_commitment_number ctx)

    untrimmed = filter (not . is_trimmed dust rate fmt) (cc_htlcs ctx)

    !fee = commitment_fee rate fmt (fromIntegral (length untrimmed))
    !deduction = case fmt of
      StaticRemotekey -> fee
      Anchors         ->
        fromMaybe BOLT1.max_satoshi (BOLT1.add_sat fee (sat 660))
    deduct s = fromMaybe (sat 0) (BOLT1.sub_sat s deduction)

    !local_sat  = BOLT1.msat_to_sat (cc_to_local_msat ctx)
    !remote_sat = BOLT1.msat_to_sat (cc_to_remote_msat ctx)
    (!to_local_sat, !to_remote_sat)
      | cc_is_funder ctx = (deduct local_sat, remote_sat)
      | otherwise        = (local_sat, deduct remote_sat)

    to_local =
      [ CommitmentOutput to_local_sat
          (to_p2wsh (to_local_script (ck_revocation_pubkey keys)
                       (cc_to_self_delay ctx) (ck_local_delayed keys)))
          OutputToLocal
      | to_local_sat >= dust_sat ]

    to_remote =
      [ CommitmentOutput to_remote_sat
          (to_remote_script_pubkey (ck_remote_payment keys) fmt)
          OutputToRemote
      | to_remote_sat >= dust_sat ]

    has_htlcs = not (null untrimmed)
    anchor pk ty = CommitmentOutput anchor_output_value
      (to_p2wsh (anchor_script pk)) ty

    local_anchor = case fmt of
      Anchors | not (null to_local) || has_htlcs ->
        [anchor (ck_local_funding keys) OutputLocalAnchor]
      _ -> []

    remote_anchor = case fmt of
      Anchors | not (null to_remote) || has_htlcs ->
        [anchor (ck_remote_funding keys) OutputRemoteAnchor]
      _ -> []

    htlcs = fmap htlc_output untrimmed

    htlc_output h =
      let !script = case htlc_direction h of
            HTLCOffered  -> offered_htlc_script
              (ck_revocation_pubkey keys) (ck_remote_htlc keys)
              (ck_local_htlc keys) (htlc_payment_hash h) fmt
            HTLCReceived -> received_htlc_script
              (ck_revocation_pubkey keys) (ck_remote_htlc keys)
              (ck_local_htlc keys) (htlc_payment_hash h)
              (htlc_cltv_expiry h) fmt
      in  CommitmentOutput (BOLT1.msat_to_sat (htlc_amount_msat h))
            (to_p2wsh script) (OutputHTLC h)

-- BIP69+CLTV order: value, then scriptPubKey, then (for HTLCs) CLTV
-- expiry.
sort_outputs :: [CommitmentOutput] -> [CommitmentOutput]
sort_outputs = sortBy cmp where
  cmp a b = compare (co_value a) (co_value b)
         <> compare (co_script a) (co_script b)
         <> compare (cltv (co_type a)) (cltv (co_type b))
  cltv t = case t of
    OutputHTLC h -> Just (htlc_cltv_expiry h)
    _            -> Nothing

-- HTLC transactions ----------------------------------------------------------

-- | The parameters of an HTLC-success or HTLC-timeout transaction,
--   spending an HTLC output of the owner's commitment transaction.
data HTLCContext = HTLCContext
  { hc_commitment_txid   :: !TxId
  , hc_output_index      :: {-# UNPACK #-} !Word32
  , hc_htlc              :: !HTLC
  , hc_to_self_delay     :: {-# UNPACK #-} !ToSelfDelay
  , hc_feerate           :: {-# UNPACK #-} !FeeratePerKw
  , hc_format            :: !CommitmentFormat
  , hc_revocation_pubkey :: !RevocationPubkey
  , hc_local_delayed     :: !LocalDelayedPubkey
  } deriving (Eq, Show, Generic)

instance NFData HTLCContext

-- | An unsigned HTLC-success or HTLC-timeout transaction.
data HTLCTx = HTLCTx
  { htx_version        :: {-# UNPACK #-} !Word32
  , htx_locktime       :: {-# UNPACK #-} !Locktime
  , htx_input_outpoint :: {-# UNPACK #-} !OutPoint
  , htx_input_sequence :: {-# UNPACK #-} !Sequence
  , htx_output_value   :: {-# UNPACK #-} !Satoshi
  , htx_output_script  :: !Script
  } deriving (Eq, Show, Generic)

instance NFData HTLCTx

-- | Build the second-stage transaction for an HTLC output of the
--   owner's commitment transaction: HTLC-timeout (locktime
--   @cltv_expiry@) for an offered HTLC, and HTLC-success (locktime 0)
--   for a received one.
--
--   The output pays the HTLC amount, less the HTLC transaction fee
--   (zero under 'Anchors'), to the 'to_local_script' of the owner's
--   keys. Fails if the fee exceeds the amount (such an HTLC is always
--   trimmed).
build_htlc_tx :: HTLCContext -> Maybe HTLCTx
build_htlc_tx ctx = do
    value <- BOLT1.sub_sat (BOLT1.msat_to_sat (htlc_amount_msat h)) fee
    pure HTLCTx
      { htx_version        = 2
      , htx_locktime       = locktime
      , htx_input_outpoint =
          OutPoint (hc_commitment_txid ctx) (hc_output_index ctx)
      , htx_input_sequence = case fmt of
          StaticRemotekey -> Sequence 0
          Anchors         -> Sequence 1
      , htx_output_value   = value
      , htx_output_script  = to_p2wsh $ to_local_script
          (hc_revocation_pubkey ctx) (hc_to_self_delay ctx)
          (hc_local_delayed ctx)
      }
  where
    !h = hc_htlc ctx
    !fmt = hc_format ctx
    (!fee, !locktime) = case htlc_direction h of
      HTLCOffered  ->
        let CltvExpiry e = htlc_cltv_expiry h
        in  (htlc_timeout_fee (hc_feerate ctx) fmt, Locktime e)
      HTLCReceived ->
        (htlc_success_fee (hc_feerate ctx) fmt, Locktime 0)

-- closing transactions -------------------------------------------------------

-- | A closing transaction output.
data ClosingOutput = ClosingOutput
  { clo_value  :: {-# UNPACK #-} !Satoshi
  , clo_script :: !Script     -- ^ scriptPubKey
  } deriving (Eq, Show, Generic)

instance NFData ClosingOutput

-- | An unsigned closing transaction.
data ClosingTx = ClosingTx
  { cltx_version        :: {-# UNPACK #-} !Word32
  , cltx_locktime       :: {-# UNPACK #-} !Locktime
  , cltx_input_outpoint :: {-# UNPACK #-} !OutPoint
  , cltx_input_sequence :: {-# UNPACK #-} !Sequence
  , cltx_outputs        :: !(NonEmpty ClosingOutput)
    -- ^ in BIP69 order
  } deriving (Eq, Show, Generic)

instance NFData ClosingTx

-- | Which outputs an @option_simple_close@ closing transaction has,
--   matching the signature fields of @closing_complete@.
data ClosingOutputs
  = CloserOutputOnly
  | CloseeOutputOnly
  | CloserAndCloseeOutputs
  deriving (Eq, Show, Generic)

instance NFData ClosingOutputs

-- | The parameters of an @option_simple_close@ closing transaction,
--   as given by a @closing_complete@ message.
data ClosingContext = ClosingContext
  { clc_funding_outpoint :: {-# UNPACK #-} !OutPoint
  , clc_closer_msat      :: {-# UNPACK #-} !MilliSatoshi
    -- ^ the closer's final balance
  , clc_closee_msat      :: {-# UNPACK #-} !MilliSatoshi
    -- ^ the closee's final balance
  , clc_closer_script    :: !Script
  , clc_closee_script    :: !Script
  , clc_fee              :: {-# UNPACK #-} !Satoshi
  , clc_locktime         :: {-# UNPACK #-} !Locktime
  , clc_outputs          :: !ClosingOutputs
  } deriving (Eq, Show, Generic)

instance NFData ClosingContext

-- | Build an @option_simple_close@ closing transaction (BOLT #3's
--   "Closing Transaction"), for @closing_complete@ and @closing_sig@.
--
--   The closer pays the fee: its output is its balance, rounded down
--   to whole satoshis, less the fee. An output whose scriptPubKey
--   starts with @OP_RETURN@ has amount 0. Only the chosen outputs are
--   included, and none is trimmed. Fails if the fee exceeds the
--   closer's balance.
build_closing_tx :: ClosingContext -> Maybe ClosingTx
build_closing_tx ctx = do
    closer_sat <-
      BOLT1.sub_sat (BOLT1.msat_to_sat (clc_closer_msat ctx)) (clc_fee ctx)
    let !closer = output closer_sat (clc_closer_script ctx)
        !closee = output (BOLT1.msat_to_sat (clc_closee_msat ctx))
                    (clc_closee_script ctx)
    pure ClosingTx
      { cltx_version        = 2
      , cltx_locktime       = clc_locktime ctx
      , cltx_input_outpoint = clc_funding_outpoint ctx
      , cltx_input_sequence = Sequence 0xFFFFFFFD
      , cltx_outputs        = case clc_outputs ctx of
          CloserOutputOnly       -> closer :| []
          CloseeOutputOnly       -> closee :| []
          CloserAndCloseeOutputs -> sort_closing (closer :| [closee])
      }
  where
    output amt spk@(Script s) = case BS.uncons s of
      Just (0x6a, _) -> ClosingOutput (sat 0) spk
      _              -> ClosingOutput amt spk

-- | The parameters of a legacy closing transaction (for
--   @closing_signed@), from the point of view of the node producing
--   the signature (the "local" party).
data LegacyClosingContext = LegacyClosingContext
  { lcc_funding_outpoint :: {-# UNPACK #-} !OutPoint
  , lcc_local_msat       :: {-# UNPACK #-} !MilliSatoshi
  , lcc_remote_msat      :: {-# UNPACK #-} !MilliSatoshi
  , lcc_local_script     :: !Script
  , lcc_remote_script    :: !Script
  , lcc_dust_limit       :: {-# UNPACK #-} !DustLimit
    -- ^ the signer's @dust_limit_satoshis@
  , lcc_fee              :: {-# UNPACK #-} !Satoshi
  , lcc_is_funder        :: !Bool
    -- ^ whether the signer funded the channel, and so pays the fee
  , lcc_omit_local       :: !Bool
    -- ^ whether the signer eliminates its own output
  } deriving (Eq, Show, Generic)

instance NFData LegacyClosingContext

-- | Build a legacy closing transaction (BOLT #3's "Legacy Closing
--   Transaction"), for @closing_signed@.
--
--   Balances are rounded down to whole satoshis and the fee is taken
--   from the funder's output. Outputs below the signer's dust limit
--   are removed, as is the signer's own output if 'lcc_omit_local' is
--   set. Fails if the fee exceeds the funder's balance, or if no
--   output remains.
build_legacy_closing_tx :: LegacyClosingContext -> Maybe ClosingTx
build_legacy_closing_tx ctx = do
    let !local_sat  = BOLT1.msat_to_sat (lcc_local_msat ctx)
        !remote_sat = BOLT1.msat_to_sat (lcc_remote_msat ctx)
        DustLimit dust = lcc_dust_limit ctx
    (l, r) <-
      if   lcc_is_funder ctx
      then fmap (\x -> (x, remote_sat)) (BOLT1.sub_sat local_sat fee)
      else fmap (\x -> (local_sat, x)) (BOLT1.sub_sat remote_sat fee)
    outputs <- NE.nonEmpty $
         [ ClosingOutput l (lcc_local_script ctx)
         | not (lcc_omit_local ctx), l >= dust ]
      <> [ ClosingOutput r (lcc_remote_script ctx) | r >= dust ]
    pure ClosingTx
      { cltx_version        = 2
      , cltx_locktime       = Locktime 0
      , cltx_input_outpoint = lcc_funding_outpoint ctx
      , cltx_input_sequence = Sequence 0xFFFFFFFF
      , cltx_outputs        = sort_closing outputs
      }
  where
    !fee = lcc_fee ctx

-- BIP69 order: value, then scriptPubKey.
sort_closing :: NonEmpty ClosingOutput -> NonEmpty ClosingOutput
sort_closing = NE.sortBy $ \a b ->
  compare (clo_value a) (clo_value b) <> compare (clo_script a) (clo_script b)

-- fees -----------------------------------------------------------------------

-- feerate * weight / 1000, saturating at 21 million BTC.
weight_fee :: FeeratePerKw -> Integer -> Satoshi
weight_fee (FeeratePerKw rate) weight =
  let !f = toInteger rate * weight `quot` 1000
  in  if   f > toInteger (BOLT1.un_satoshi BOLT1.max_satoshi)
      then BOLT1.max_satoshi
      else sat (fromInteger f)

-- | The base fee of a commitment transaction with a given number of
--   untrimmed HTLC outputs:
--
--   @feerate_per_kw * (724 + 172 * num_htlcs) / 1000@
--
--   with a base weight of 1124 rather than 724 under 'Anchors'.
--
--   >>> commitment_fee (FeeratePerKw 5000) StaticRemotekey 2
--   Satoshi 5340
commitment_fee :: FeeratePerKw -> CommitmentFormat -> Word64 -> Satoshi
commitment_fee rate fmt n = weight_fee rate (base + 172 * toInteger n) where
  base = case fmt of
    StaticRemotekey -> 724
    Anchors         -> 1124

-- | The fee of an HTLC-timeout transaction: @feerate_per_kw * 663 /
--   1000@, or zero under 'Anchors'.
--
--   >>> htlc_timeout_fee (FeeratePerKw 5000) StaticRemotekey
--   Satoshi 3315
htlc_timeout_fee :: FeeratePerKw -> CommitmentFormat -> Satoshi
htlc_timeout_fee rate fmt = case fmt of
  StaticRemotekey -> weight_fee rate 663
  Anchors         -> sat 0

-- | The fee of an HTLC-success transaction: @feerate_per_kw * 703 /
--   1000@, or zero under 'Anchors'.
--
--   >>> htlc_success_fee (FeeratePerKw 5000) StaticRemotekey
--   Satoshi 3515
htlc_success_fee :: FeeratePerKw -> CommitmentFormat -> Satoshi
htlc_success_fee rate fmt = case fmt of
  StaticRemotekey -> weight_fee rate 703
  Anchors         -> sat 0

-- trimming -------------------------------------------------------------------

-- | The amount below which an HTLC in a given direction is trimmed:
--   the dust limit plus the fee of its HTLC transaction.
--
--   >>> let Just d = fmap DustLimit (BOLT1.satoshi 546)
--   >>> htlc_trim_threshold d (FeeratePerKw 5000) StaticRemotekey HTLCOffered
--   Satoshi 3861
htlc_trim_threshold
  :: DustLimit
  -> FeeratePerKw
  -> CommitmentFormat
  -> HTLCDirection
  -> Satoshi
htlc_trim_threshold (DustLimit dust) rate fmt dir =
  let !fee = case dir of
        HTLCOffered  -> htlc_timeout_fee rate fmt
        HTLCReceived -> htlc_success_fee rate fmt
  in  fromMaybe BOLT1.max_satoshi (BOLT1.add_sat dust fee)

-- | Whether an HTLC is trimmed from a commitment transaction: whether
--   its amount, rounded down to whole satoshis, is below
--   'htlc_trim_threshold'.
is_trimmed :: DustLimit -> FeeratePerKw -> CommitmentFormat -> HTLC -> Bool
is_trimmed dust rate fmt h =
  BOLT1.msat_to_sat (htlc_amount_msat h)
    < htlc_trim_threshold dust rate fmt (htlc_direction h)

-- serialization --------------------------------------------------------------

unsigned_tx
  :: Word32 -> Locktime -> OutPoint -> Sequence -> NonEmpty BT.TxOut
  -> BT.Tx
unsigned_tx version (Locktime lt) prevout (Sequence sq) outs = BT.Tx
  { BT.tx_version   = version
  , BT.tx_inputs    = BT.TxIn prevout BS.empty sq :| []
  , BT.tx_outputs   = outs
  , BT.tx_witnesses = []
  , BT.tx_locktime  = lt
  }

txout :: Satoshi -> Script -> BT.TxOut
txout v (Script s) = BT.TxOut (BOLT1.un_satoshi v) s
{-# INLINE txout #-}

-- | The unsigned commitment transaction as a ppad-tx 'BT.Tx' (with no
--   witnesses), e.g. for computing its txid or sighashes.
commitment_to_tx :: CommitmentTx -> BT.Tx
commitment_to_tx c = unsigned_tx (ctx_version c) (ctx_locktime c)
  (ctx_input_outpoint c) (ctx_input_sequence c)
  (fmap (\o -> txout (co_value o) (co_script o)) (ctx_outputs c))

-- | The unsigned HTLC transaction as a ppad-tx 'BT.Tx'.
htlc_to_tx :: HTLCTx -> BT.Tx
htlc_to_tx h = unsigned_tx (htx_version h) (htx_locktime h)
  (htx_input_outpoint h) (htx_input_sequence h)
  (txout (htx_output_value h) (htx_output_script h) :| [])

-- | The unsigned closing transaction as a ppad-tx 'BT.Tx'.
closing_to_tx :: ClosingTx -> BT.Tx
closing_to_tx c = unsigned_tx (cltx_version c) (cltx_locktime c)
  (cltx_input_outpoint c) (cltx_input_sequence c)
  (fmap (\o -> txout (clo_value o) (clo_script o)) (cltx_outputs c))

-- | Serialize an unsigned commitment transaction, without witness
--   data. This is the form whose double-SHA256 is the txid; it is not
--   what segwit signatures commit to (see ppad-tx's
--   @Bitcoin.Prim.Tx.Sighash@). To broadcast, attach the witness to
--   'commitment_to_tx' and serialize with ppad-tx.
encode_commitment_tx :: CommitmentTx -> BS.ByteString
encode_commitment_tx = BT.to_bytes_legacy . commitment_to_tx

-- | Serialize an unsigned HTLC transaction, without witness data (cf.
--   'encode_commitment_tx').
encode_htlc_tx :: HTLCTx -> BS.ByteString
encode_htlc_tx = BT.to_bytes_legacy . htlc_to_tx

-- | Serialize an unsigned closing transaction, without witness data
--   (cf. 'encode_commitment_tx').
encode_closing_tx :: ClosingTx -> BS.ByteString
encode_closing_tx = BT.to_bytes_legacy . closing_to_tx
