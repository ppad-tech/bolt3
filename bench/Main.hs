{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq (NFData(..))
import Criterion.Main
import Data.Word (Word64)
import qualified Data.ByteString as BS
import Lightning.Protocol.BOLT3
import Lightning.Protocol.BOLT3.Types
  ( Pubkey(..), Point(..)
  , PaymentHash(..), PerCommitmentPoint(..)
  , CommitmentNumber(..)
  )

-- NFData instances for benchmarking
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

instance NFData ChannelFeatures where
  rnf (ChannelFeatures x) = rnf x

instance NFData FeeratePerKw where
  rnf (FeeratePerKw x) = rnf x

instance NFData DustLimit where
  rnf (DustLimit x) = rnf x

instance NFData CltvExpiry where
  rnf (CltvExpiry x) = rnf x

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

-- Key types
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

-- Secret storage (SecretStore is a newtype over list)
instance NFData SecretStore where
  rnf store = rnf (derive_old_secret 0 store)

-- Validation errors
instance NFData ValidationError where
  rnf (InvalidVersion a b) = rnf a `seq` rnf b
  rnf (InvalidLocktime a) = rnf a
  rnf (InvalidSequence a) = rnf a
  rnf InvalidOutputOrdering = ()
  rnf (DustLimitViolation a b c) = rnf a `seq` rnf b `seq` rnf c
  rnf MissingAnchorOutput = ()
  rnf (InvalidAnchorValue a) = rnf a
  rnf (InvalidFee a b) = rnf a `seq` rnf b
  rnf (InvalidHTLCLocktime a b) = rnf a `seq` rnf b
  rnf (InvalidHTLCSequence a b) = rnf a `seq` rnf b
  rnf NoOutputs = ()
  rnf (TooManyOutputs a) = rnf a

main :: IO ()
main = defaultMain [
    bgroup "key derivation" [
      bench "derive_pubkey" $
        whnf (derive_pubkey basepoint) perCommitmentPoint
    , bench "derive_revocationpubkey" $
        whnf (derive_revocationpubkey revocationBasepoint) perCommitmentPoint
    ]
  , bgroup "secret generation" [
      bench "generate_from_seed (final node)" $
        whnf (generate_from_seed seed) 281474976710655
    , bench "generate_from_seed (first node)" $
        whnf (generate_from_seed seed) 0
    ]
  , bgroup "fee calculation" [
      bench "commitment_fee (no anchors, 0 htlcs)" $
        whnf (commitment_fee feerate noAnchors) 0
    , bench "commitment_fee (no anchors, 10 htlcs)" $
        whnf (commitment_fee feerate noAnchors) 10
    , bench "commitment_fee (anchors, 10 htlcs)" $
        whnf (commitment_fee feerate withAnchors) 10
    , bench "htlc_timeout_fee" $
        whnf (htlc_timeout_fee feerate) noAnchors
    , bench "htlc_success_fee" $
        whnf (htlc_success_fee feerate) noAnchors
    ]
  , bgroup "trimming" [
      bench "is_trimmed (offered, not trimmed)" $
        whnf (is_trimmed dust feerate noAnchors) htlcNotTrimmed
    , bench "is_trimmed (offered, trimmed)" $
        whnf (is_trimmed dust feerate noAnchors) htlcTrimmed
    , bench "htlc_trim_threshold (offered)" $
        whnf (htlc_trim_threshold dust feerate noAnchors) HTLCOffered
    ]
  , bgroup "tx building" [
      bench "build_commitment_tx (0 htlcs, no anchors)" $
        whnf build_commitment_tx (mkCommitmentContext htlcs0 noAnchors)
    , bench "build_commitment_tx (10 htlcs, no anchors)" $
        whnf build_commitment_tx (mkCommitmentContext htlcs10 noAnchors)
    , bench "build_commitment_tx (100 htlcs, no anchors)" $
        whnf build_commitment_tx (mkCommitmentContext htlcs100 noAnchors)
    , bench "build_commitment_tx (10 htlcs, anchors)" $
        whnf build_commitment_tx (mkCommitmentContext htlcs10 withAnchors)
    , bench "build_htlc_timeout_tx" $
        whnf build_htlc_timeout_tx sampleHtlcContext
    , bench "build_htlc_success_tx" $
        whnf build_htlc_success_tx sampleHtlcContext
    , bench "build_closing_tx" $
        whnf build_closing_tx sampleClosingContext
    ]
  , bgroup "script generation" [
      bench "funding_script" $
        whnf (funding_script (FundingPubkey samplePubkey1))
             (FundingPubkey samplePubkey2)
    , bench "to_local_script" $
        whnf (to_local_script (RevocationPubkey samplePubkey1)
                              (ToSelfDelay 144))
             (LocalDelayedPubkey samplePubkey2)
    , bench "to_remote_script (no anchors)" $
        whnf (to_remote_script (RemotePubkey samplePubkey1)) noAnchors
    , bench "to_remote_script (anchors)" $
        whnf (to_remote_script (RemotePubkey samplePubkey1)) withAnchors
    , bench "anchor_script" $
        whnf anchor_script (FundingPubkey samplePubkey1)
    , bench "offered_htlc_script" $
        whnf (offered_htlc_script (RevocationPubkey samplePubkey1)
                                  (RemoteHtlcPubkey samplePubkey2)
                                  (LocalHtlcPubkey samplePubkey3)
                                  (PaymentHash $ BS.replicate 32 0))
             noAnchors
    , bench "received_htlc_script" $
        whnf (received_htlc_script (RevocationPubkey samplePubkey1)
                                   (RemoteHtlcPubkey samplePubkey2)
                                   (LocalHtlcPubkey samplePubkey3)
                                   (PaymentHash $ BS.replicate 32 0)
                                   (CltvExpiry 500000))
             noAnchors
    ]
  , bgroup "serialization" [
      env (pure $ build_commitment_tx $ mkCommitmentContext htlcs0 noAnchors)
        $ \tx -> bench "encode_tx (0 htlcs)" $ whnf encode_tx tx
    , env (pure $ build_commitment_tx $ mkCommitmentContext htlcs10 noAnchors)
        $ \tx -> bench "encode_tx (10 htlcs)" $ whnf encode_tx tx
    , env (pure $ build_commitment_tx $ mkCommitmentContext htlcs100 noAnchors)
        $ \tx -> bench "encode_tx (100 htlcs)" $ whnf encode_tx tx
    , bench "encode_htlc_tx" $
        whnf encode_htlc_tx (build_htlc_timeout_tx sampleHtlcContext)
    , bench "encode_closing_tx" $
        whnf encode_closing_tx (build_closing_tx sampleClosingContext)
    ]
  , bgroup "parsing" [
      env (pure $ encodeTx0 htlcs0 noAnchors)
        $ \bs -> bench "decode_tx (0 htlcs)" $ whnf decode_tx bs
    , env (pure $ encodeTx0 htlcs10 noAnchors)
        $ \bs -> bench "decode_tx (10 htlcs)" $ whnf decode_tx bs
    , env (pure $ encodeTx0 htlcs100 noAnchors)
        $ \bs -> bench "decode_tx (100 htlcs)" $ whnf decode_tx bs
    ]
  , bgroup "validation" [
      env (pure $ build_commitment_tx $ mkCommitmentContext htlcs10 noAnchors)
        $ \tx -> bench "validate_commitment_tx (valid)" $
            whnf (validate_commitment_tx dust noAnchors) tx
    , env (pure $ build_htlc_timeout_tx sampleHtlcContext)
        $ \tx -> bench "validate_htlc_tx" $
            whnf validate_htlc_tx tx
    , env (pure $ build_closing_tx sampleClosingContext)
        $ \tx -> bench "validate_closing_tx" $
            whnf validate_closing_tx tx
    , env (pure $ ctx_outputs $ build_commitment_tx $
             mkCommitmentContext htlcs10 noAnchors)
        $ \outs -> bench "validate_output_ordering" $
            whnf validate_output_ordering outs
    ]
  , bgroup "secret storage" [
      bench "insert_secret (first)" $
        whnf (insert_secret (BS.replicate 32 0xFF) 281474976710655)
             empty_store
    , env setupFilledStore $ \store ->
        bench "derive_old_secret (recent)" $
          whnf (derive_old_secret 281474976710654) store
    , env setupFilledStore $ \store ->
        bench "derive_old_secret (old)" $
          whnf (derive_old_secret 281474976710600) store
    ]
  , bgroup "output sorting" [
      env (pure $ ctx_outputs $ build_commitment_tx $
             mkCommitmentContext htlcs10 noAnchors)
        $ \outs -> bench "sort_outputs (10)" $ nf sort_outputs outs
    , env (pure $ ctx_outputs $ build_commitment_tx $
             mkCommitmentContext htlcs100 noAnchors)
        $ \outs -> bench "sort_outputs (100)" $ nf sort_outputs outs
    ]
  ]
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

    -- HTLC lists
    mkHtlc :: HTLCDirection -> Word64 -> Word64 -> HTLC
    mkHtlc dir amtMsat expiry = HTLC
      { htlc_direction = dir
      , htlc_amount_msat = MilliSatoshi amtMsat
      , htlc_payment_hash = PaymentHash (BS.replicate 32 0x00)
      , htlc_cltv_expiry = CltvExpiry (fromIntegral expiry)
      }

    htlcs0, htlcs10, htlcs100 :: [HTLC]
    htlcs0 = []
    htlcs10 = [mkHtlc (if even i then HTLCOffered else HTLCReceived)
                      (5000000 + i * 100000) (500000 + i)
              | i <- [0..9]]
    htlcs100 = [mkHtlc (if even i then HTLCOffered else HTLCReceived)
                       (5000000 + i * 10000) (500000 + i)
               | i <- [0..99]]

    -- CommitmentKeys fixture
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
      , cc_local_payment_bp =
          PaymentBasepoint $ Point $ unPubkey samplePubkey1
      , cc_remote_payment_bp =
          PaymentBasepoint $ Point $ unPubkey samplePubkey2
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
      , clc_local_script =
          Script $ BS.pack [0x00, 0x14] <> BS.replicate 20 0x01
      , clc_remote_script =
          Script $ BS.pack [0x00, 0x14] <> BS.replicate 20 0x02
      , clc_local_dust_limit = DustLimit (Satoshi 546)
      , clc_remote_dust_limit = DustLimit (Satoshi 546)
      , clc_fee = Satoshi 1000
      , clc_is_funder = True
      , clc_locktime = Locktime 0
      , clc_funding_script = funding_script (FundingPubkey samplePubkey1)
                                            (FundingPubkey samplePubkey2)
      }

    -- Setup for secret storage benchmarks
    setupFilledStore :: IO SecretStore
    setupFilledStore = do
      let secrets = [(generate_from_seed seed i, i)
                    | i <- [281474976710655, 281474976710654 .. 281474976710600]]
      pure $! foldl insertOrFail empty_store secrets
      where
        insertOrFail store (sec, idx) =
          case insert_secret sec idx store of
            Just s  -> s
            Nothing -> store
