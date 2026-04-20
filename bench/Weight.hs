{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq (NFData(..))
import qualified Data.ByteString as BS
import Data.Word (Word32, Word64)
import Lightning.Protocol.BOLT3
import Lightning.Protocol.BOLT3.Types
  ( Pubkey(..), Point(..)
  , PaymentHash(..), PerCommitmentPoint(..)
  , CommitmentNumber(..)
  )
import Weigh

-- NFData instances for weigh
-- (Satoshi, MilliSatoshi, Point, PaymentHash, PerCommitmentSecret
-- derive NFData via ppad-bolt1)

instance NFData Pubkey where
  rnf (Pubkey x) = rnf x

instance NFData PerCommitmentPoint where
  rnf (PerCommitmentPoint x) = rnf x

instance NFData RevocationPubkey where
  rnf (RevocationPubkey x) = rnf x

instance NFData RevocationBasepoint where
  rnf (RevocationBasepoint x) = rnf x

instance NFData LocalDelayedPubkey where
  rnf (LocalDelayedPubkey p) = rnf p

instance NFData RemoteDelayedPubkey where
  rnf (RemoteDelayedPubkey p) = rnf p

instance NFData LocalHtlcPubkey where
  rnf (LocalHtlcPubkey p) = rnf p

instance NFData RemoteHtlcPubkey where
  rnf (RemoteHtlcPubkey p) = rnf p

instance NFData LocalPubkey where
  rnf (LocalPubkey p) = rnf p

instance NFData RemotePubkey where
  rnf (RemotePubkey p) = rnf p

instance NFData PaymentBasepoint where
  rnf (PaymentBasepoint p) = rnf p

instance NFData DelayedPaymentBasepoint where
  rnf (DelayedPaymentBasepoint p) = rnf p

instance NFData HtlcBasepoint where
  rnf (HtlcBasepoint p) = rnf p

instance NFData FundingPubkey where
  rnf (FundingPubkey p) = rnf p

-- Channel features
instance NFData ChannelFeatures where
  rnf (ChannelFeatures x) = rnf x

instance NFData FeeratePerKw where
  rnf (FeeratePerKw x) = rnf x

instance NFData DustLimit where
  rnf (DustLimit x) = rnf x

-- Hash types
instance NFData CltvExpiry where
  rnf (CltvExpiry x) = rnf x

-- HTLC types
instance NFData HTLCDirection where
  rnf HTLCOffered = ()
  rnf HTLCReceived = ()

instance NFData HTLC where
  rnf (HTLC d a h c) = rnf d `seq` rnf a `seq` rnf h `seq` rnf c

-- Transaction types
instance NFData CommitmentTx where
  rnf (CommitmentTx v l i s o f) =
    rnf v `seq` rnf l `seq` rnf i `seq` rnf s `seq` rnf o `seq` rnf f

instance NFData HTLCTx where
  rnf (HTLCTx v l i s ov os) =
    rnf v `seq` rnf l `seq` rnf i `seq` rnf s `seq` rnf ov `seq` rnf os

instance NFData ClosingTx where
  rnf (ClosingTx v l i s o f) =
    rnf v `seq` rnf l `seq` rnf i `seq` rnf s `seq` rnf o `seq` rnf f

-- Output types
instance NFData TxOutput where
  rnf (TxOutput v s t) = rnf v `seq` rnf s `seq` rnf t

instance NFData OutputType where
  rnf OutputToLocal = ()
  rnf OutputToRemote = ()
  rnf OutputLocalAnchor = ()
  rnf OutputRemoteAnchor = ()
  rnf (OutputOfferedHTLC e) = rnf e
  rnf (OutputReceivedHTLC e) = rnf e

-- Primitives
instance NFData Script where
  rnf (Script bs) = rnf bs


instance NFData Sequence where
  rnf (Sequence x) = rnf x

instance NFData Locktime where
  rnf (Locktime x) = rnf x

instance NFData ToSelfDelay where
  rnf (ToSelfDelay x) = rnf x

instance NFData CommitmentNumber where
  rnf (CommitmentNumber x) = rnf x

-- Context types
instance NFData CommitmentContext where
  rnf ctx = rnf (cc_funding_outpoint ctx) `seq`
            rnf (cc_commitment_number ctx) `seq`
            rnf (cc_htlcs ctx) `seq`
            rnf (cc_keys ctx)

instance NFData CommitmentKeys where
  rnf keys = rnf (ck_revocation_pubkey keys) `seq`
             rnf (ck_local_delayed keys) `seq`
             rnf (ck_local_htlc keys) `seq`
             rnf (ck_remote_htlc keys)

instance NFData HTLCContext where
  rnf ctx = rnf (hc_commitment_txid ctx) `seq`
            rnf (hc_htlc ctx)

instance NFData ClosingContext where
  rnf ctx = rnf (clc_funding_outpoint ctx) `seq`
            rnf (clc_local_amount ctx) `seq`
            rnf (clc_remote_amount ctx)

instance NFData ValidationError where
  rnf (InvalidVersion e a) = rnf e `seq` rnf a
  rnf (InvalidLocktime lt) = rnf lt
  rnf (InvalidSequence sq) = rnf sq
  rnf InvalidOutputOrdering = ()
  rnf (DustLimitViolation i v d) = rnf i `seq` rnf v `seq` rnf d
  rnf MissingAnchorOutput = ()
  rnf (InvalidAnchorValue v) = rnf v
  rnf (InvalidFee e a) = rnf e `seq` rnf a
  rnf (InvalidHTLCLocktime e a) = rnf e `seq` rnf a
  rnf (InvalidHTLCSequence e a) = rnf e `seq` rnf a
  rnf NoOutputs = ()
  rnf (TooManyOutputs n) = rnf n

-- Secret store (opaque, use generic rnf on the list)
instance NFData SecretStore where
  rnf ss = ss `seq` ()

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]

  -- Key derivation allocations
  func "derive_pubkey" (derive_pubkey basepoint) perCommitmentPoint
  func "derive_revocationpubkey"
       (derive_revocationpubkey revocationBasepoint) perCommitmentPoint

  -- Secret generation allocations
  func "generate_from_seed (final)" (generate_from_seed seed) 281474976710655
  func "generate_from_seed (first)" (generate_from_seed seed) 0

  -- Fee calculation allocations
  func "commitment_fee (0 htlcs)" (commitment_fee feerate noAnchors) 0
  func "commitment_fee (10 htlcs)" (commitment_fee feerate noAnchors) 10
  func "htlc_timeout_fee" (htlc_timeout_fee feerate) noAnchors
  func "htlc_success_fee" (htlc_success_fee feerate) noAnchors

  -- Trimming allocations
  func "is_trimmed (not trimmed)"
       (is_trimmed dust feerate noAnchors) htlcNotTrimmed
  func "is_trimmed (trimmed)"
       (is_trimmed dust feerate noAnchors) htlcTrimmed
  func "htlc_trim_threshold"
       (htlc_trim_threshold dust feerate noAnchors) HTLCOffered

  -- Transaction building allocations
  func "build_commitment_tx (0 htlcs, no anchors)"
       build_commitment_tx (mkCommitmentContext htlcs0 noAnchors)
  func "build_commitment_tx (10 htlcs, no anchors)"
       build_commitment_tx (mkCommitmentContext htlcs10 noAnchors)
  func "build_commitment_tx (100 htlcs, no anchors)"
       build_commitment_tx (mkCommitmentContext htlcs100 noAnchors)
  func "build_commitment_tx (10 htlcs, anchors)"
       build_commitment_tx (mkCommitmentContext htlcs10 withAnchors)
  func "build_htlc_timeout_tx"
       build_htlc_timeout_tx sampleHtlcContext
  func "build_htlc_success_tx"
       build_htlc_success_tx sampleHtlcContext
  func "build_closing_tx"
       build_closing_tx sampleClosingContext

  -- Script generation allocations
  func "funding_script"
       (funding_script (FundingPubkey samplePubkey1))
       (FundingPubkey samplePubkey2)
  func "to_local_script"
       (to_local_script (RevocationPubkey samplePubkey1)
                        (ToSelfDelay 144))
       (LocalDelayedPubkey samplePubkey2)
  func "to_remote_script (no anchors)"
       (to_remote_script (RemotePubkey samplePubkey1)) noAnchors
  func "to_remote_script (anchors)"
       (to_remote_script (RemotePubkey samplePubkey1)) withAnchors
  func "anchor_script"
       anchor_script (FundingPubkey samplePubkey1)
  func "offered_htlc_script"
       (offered_htlc_script (RevocationPubkey samplePubkey1)
                            (RemoteHtlcPubkey samplePubkey2)
                            (LocalHtlcPubkey samplePubkey3)
                            (PaymentHash $ BS.replicate 32 0))
       noAnchors
  func "received_htlc_script"
       (received_htlc_script (RevocationPubkey samplePubkey1)
                             (RemoteHtlcPubkey samplePubkey2)
                             (LocalHtlcPubkey samplePubkey3)
                             (PaymentHash $ BS.replicate 32 0)
                             (CltvExpiry 500000))
       noAnchors

  -- Serialization allocations
  func "encode_tx (0 htlcs)"
       encode_tx (build_commitment_tx $
                    mkCommitmentContext htlcs0 noAnchors)
  func "encode_tx (10 htlcs)"
       encode_tx (build_commitment_tx $
                    mkCommitmentContext htlcs10 noAnchors)
  func "encode_tx (100 htlcs)"
       encode_tx (build_commitment_tx $
                    mkCommitmentContext htlcs100 noAnchors)
  func "encode_htlc_tx"
       encode_htlc_tx (build_htlc_timeout_tx sampleHtlcContext)
  func "encode_closing_tx"
       encode_closing_tx (build_closing_tx sampleClosingContext)

  -- Parsing allocations
  func "decode_tx (0 htlcs)"
       decode_tx (encodeTx0 htlcs0 noAnchors)
  func "decode_tx (10 htlcs)"
       decode_tx (encodeTx0 htlcs10 noAnchors)
  func "decode_tx (100 htlcs)"
       decode_tx (encodeTx0 htlcs100 noAnchors)

  -- Validation allocations
  func "validate_commitment_tx (valid)"
       (validate_commitment_tx dust noAnchors)
       (build_commitment_tx $ mkCommitmentContext htlcs10 noAnchors)
  func "validate_htlc_tx"
       validate_htlc_tx (build_htlc_timeout_tx sampleHtlcContext)
  func "validate_closing_tx"
       validate_closing_tx (build_closing_tx sampleClosingContext)
  func "validate_output_ordering"
       validate_output_ordering (ctx_outputs $ build_commitment_tx $
                                   mkCommitmentContext htlcs10 noAnchors)

  -- Secret storage allocations
  func "insert_secret (first)"
       (insert_secret (BS.replicate 32 0xFF) 281474976710655)
       empty_store
  func "derive_old_secret (recent)"
       (derive_old_secret 281474976710654)
       filledStore
  func "derive_old_secret (old)"
       (derive_old_secret 281474976710600)
       filledStore

  -- Output sorting allocations
  func "sort_outputs (10)"
       sort_outputs (ctx_outputs $ build_commitment_tx $
                       mkCommitmentContext htlcs10 noAnchors)
  func "sort_outputs (100)"
       sort_outputs (ctx_outputs $ build_commitment_tx $
                       mkCommitmentContext htlcs100 noAnchors)

  where
    -- Key derivation test data
    basepoint = Point $ BS.pack
      [0x03, 0x6d, 0x6c, 0xaa, 0xc2, 0x48, 0xaf, 0x96, 0xf6, 0xaf, 0xa7,
       0xf9, 0x04, 0xf5, 0x50, 0x25, 0x3a, 0x0f, 0x3e, 0xf3, 0xf5, 0xaa,
       0x2f, 0xe6, 0x83, 0x8a, 0x95, 0xb2, 0x16, 0x69, 0x14, 0x68, 0xe2]

    perCommitmentPoint = PerCommitmentPoint $ Point $ BS.pack
      [0x02, 0x5f, 0x71, 0x17, 0xa7, 0x81, 0x50, 0xfe, 0x2e, 0xf9, 0x7d,
       0xb7, 0xcf, 0xc8, 0x3b, 0xd5, 0x7b, 0x2e, 0x2c, 0x0d, 0x0d, 0xd2,
       0x5e, 0xaf, 0x46, 0x7a, 0x4a, 0x1c, 0x2a, 0x45, 0xce, 0x14, 0x86]

    revocationBasepoint = RevocationBasepoint $ Point $ BS.pack
      [0x03, 0x6d, 0x6c, 0xaa, 0xc2, 0x48, 0xaf, 0x96, 0xf6, 0xaf, 0xa7,
       0xf9, 0x04, 0xf5, 0x50, 0x25, 0x3a, 0x0f, 0x3e, 0xf3, 0xf5, 0xaa,
       0x2f, 0xe6, 0x83, 0x8a, 0x95, 0xb2, 0x16, 0x69, 0x14, 0x68, 0xe2]

    -- Secret generation test data
    seed = BS.replicate 32 0xFF

    -- Fee calculation test data
    feerate = FeeratePerKw 5000
    noAnchors = ChannelFeatures { cf_option_anchors = False }
    withAnchors = ChannelFeatures { cf_option_anchors = True }

    -- Trimming test data
    dust = DustLimit (Satoshi 546)

    htlcNotTrimmed = HTLC
      { htlc_direction = HTLCOffered
      , htlc_amount_msat = MilliSatoshi 5000000
      , htlc_payment_hash = PaymentHash (BS.replicate 32 0)
      , htlc_cltv_expiry = CltvExpiry 500000
      }

    htlcTrimmed = HTLC
      { htlc_direction = HTLCOffered
      , htlc_amount_msat = MilliSatoshi 1000000
      , htlc_payment_hash = PaymentHash (BS.replicate 32 0)
      , htlc_cltv_expiry = CltvExpiry 500000
      }

    -- Sample pubkeys
    samplePubkey1, samplePubkey2, samplePubkey3 :: Pubkey
    samplePubkey1 = Pubkey $ BS.pack
      [0x03, 0x6d, 0x6c, 0xaa, 0xc2, 0x48, 0xaf, 0x96, 0xf6, 0xaf, 0xa7,
       0xf9, 0x04, 0xf5, 0x50, 0x25, 0x3a, 0x0f, 0x3e, 0xf3, 0xf5, 0xaa,
       0x2f, 0xe6, 0x83, 0x8a, 0x95, 0xb2, 0x16, 0x69, 0x14, 0x68, 0xe2]
    samplePubkey2 = Pubkey $ BS.pack
      [0x02, 0x5f, 0x71, 0x17, 0xa7, 0x81, 0x50, 0xfe, 0x2e, 0xf9, 0x7d,
       0xb7, 0xcf, 0xc8, 0x3b, 0xd5, 0x7b, 0x2e, 0x2c, 0x0d, 0x0d, 0xd2,
       0x5e, 0xaf, 0x46, 0x7a, 0x4a, 0x1c, 0x2a, 0x45, 0xce, 0x14, 0x86]
    samplePubkey3 = samplePubkey1

    -- Helper to encode a commitment tx for decode benchmarks
    encodeTx0 :: [HTLC] -> ChannelFeatures -> BS.ByteString
    encodeTx0 htlcs features =
      case encode_tx (build_commitment_tx
             (mkCommitmentContext htlcs features)) of
        Nothing -> BS.empty
        Just bs -> bs

    -- Funding outpoint
    sampleFundingOutpoint :: OutPoint
    sampleFundingOutpoint = OutPoint (TxId $ BS.replicate 32 0x01) 0

    -- HTLC builder
    mkHtlc :: HTLCDirection -> Word64 -> Word32 -> HTLC
    mkHtlc dir amtMsat expiry = HTLC
      { htlc_direction = dir
      , htlc_amount_msat = MilliSatoshi amtMsat
      , htlc_payment_hash = PaymentHash (BS.replicate 32 0x00)
      , htlc_cltv_expiry = CltvExpiry expiry
      }

    htlcs0, htlcs10, htlcs100 :: [HTLC]
    htlcs0 = []
    htlcs10 = [mkHtlc (if even i then HTLCOffered else HTLCReceived)
                      (5000000 + i * 100000) (500000 + fromIntegral i)
              | i <- [0..9]]
    htlcs100 = [mkHtlc (if even i then HTLCOffered else HTLCReceived)
                       (5000000 + i * 10000) (500000 + fromIntegral i)
               | i <- [0..99]]

    -- CommitmentKeys
    sampleCommitmentKeys :: CommitmentKeys
    sampleCommitmentKeys = CommitmentKeys
      { ck_revocation_pubkey = RevocationPubkey samplePubkey1
      , ck_local_delayed = LocalDelayedPubkey samplePubkey1
      , ck_local_htlc = LocalHtlcPubkey samplePubkey1
      , ck_remote_htlc = RemoteHtlcPubkey samplePubkey2
      , ck_local_payment = LocalPubkey samplePubkey1
      , ck_remote_payment = RemotePubkey samplePubkey2
      , ck_local_funding = FundingPubkey samplePubkey1
      , ck_remote_funding = FundingPubkey samplePubkey2
      }

    -- CommitmentContext builder
    mkCommitmentContext :: [HTLC] -> ChannelFeatures -> CommitmentContext
    mkCommitmentContext htlcs features = CommitmentContext
      { cc_funding_outpoint = sampleFundingOutpoint
      , cc_commitment_number = CommitmentNumber 42
      , cc_local_payment_bp = PaymentBasepoint $
          Point $ unPubkey samplePubkey1
      , cc_remote_payment_bp = PaymentBasepoint $
          Point $ unPubkey samplePubkey2
      , cc_to_self_delay = ToSelfDelay 144
      , cc_dust_limit = DustLimit (Satoshi 546)
      , cc_feerate = FeeratePerKw 5000
      , cc_features = features
      , cc_is_funder = True
      , cc_to_local_msat = MilliSatoshi 500000000
      , cc_to_remote_msat = MilliSatoshi 500000000
      , cc_htlcs = htlcs
      , cc_keys = sampleCommitmentKeys
      }

    -- HTLC context
    sampleHtlcContext :: HTLCContext
    sampleHtlcContext = HTLCContext
      { hc_commitment_txid = TxId $ BS.replicate 32 0x01
      , hc_output_index = 0
      , hc_htlc = mkHtlc HTLCOffered 5000000 500000
      , hc_to_self_delay = ToSelfDelay 144
      , hc_feerate = FeeratePerKw 5000
      , hc_features = noAnchors
      , hc_revocation_pubkey = RevocationPubkey samplePubkey1
      , hc_local_delayed = LocalDelayedPubkey samplePubkey1
      }

    -- Closing context
    sampleClosingContext :: ClosingContext
    sampleClosingContext = ClosingContext
      { clc_funding_outpoint = sampleFundingOutpoint
      , clc_local_amount = Satoshi 500000
      , clc_remote_amount = Satoshi 500000
      , clc_local_script = Script $
          BS.pack [0x00, 0x14] <> BS.replicate 20 0x01
      , clc_remote_script = Script $
          BS.pack [0x00, 0x14] <> BS.replicate 20 0x02
      , clc_local_dust_limit = DustLimit (Satoshi 546)
      , clc_remote_dust_limit = DustLimit (Satoshi 546)
      , clc_fee = Satoshi 1000
      , clc_is_funder = True
      , clc_locktime = Locktime 0
      , clc_funding_script = funding_script (FundingPubkey samplePubkey1)
                                            (FundingPubkey samplePubkey2)
      }

    -- Secret storage for benchmarks
    filledStore :: SecretStore
    filledStore = foldl insertOne empty_store [0..99]
      where
        insertOne store i =
          let idx = 281474976710655 - i
              sec = BS.replicate 32 (fromIntegral i)
          in case insert_secret sec idx store of
               Just s -> s
               Nothing -> store
