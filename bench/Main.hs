{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.DeepSeq (NFData(..))
import Criterion.Main
import qualified Data.ByteString as BS
import Lightning.Protocol.BOLT3

-- NFData instances for benchmarking
instance NFData Satoshi where
  rnf (Satoshi x) = rnf x

instance NFData MilliSatoshi where
  rnf (MilliSatoshi x) = rnf x

instance NFData Pubkey where
  rnf (Pubkey x) = rnf x

instance NFData Point where
  rnf (Point x) = rnf x

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

instance NFData PaymentHash where
  rnf (PaymentHash x) = rnf x

instance NFData CltvExpiry where
  rnf (CltvExpiry x) = rnf x

instance NFData HTLCDirection where
  rnf HTLCOffered = ()
  rnf HTLCReceived = ()

instance NFData HTLC where
  rnf (HTLC d a h c) = rnf d `seq` rnf a `seq` rnf h `seq` rnf c

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
