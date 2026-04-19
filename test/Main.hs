{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (isJust, isNothing)
import Test.Tasty
import Test.Tasty.HUnit
import Lightning.Protocol.BOLT3

main :: IO ()
main = defaultMain $ testGroup "ppad-bolt3" [
    testGroup "Key derivation" [
      keyDerivationTests
    ]
  , testGroup "Secret generation" [
      secretGenerationTests
    ]
  , testGroup "Secret storage" [
      secretStorageTests
    ]
  , testGroup "Fee calculation" [
      feeCalculationTests
    ]
  , testGroup "Trimming" [
      trimmingTests
    ]
  , testGroup "Smart constructors" [
      smartConstructorTests
    ]
  ]

-- hex decoding helper
hex :: BS.ByteString -> BS.ByteString
hex h = case B16.decode h of
  Right bs -> bs
  Left _ -> error "invalid hex"

-- Key derivation test vectors from Appendix E ---------------------------------

keyDerivationTests :: TestTree
keyDerivationTests = testGroup "BOLT #3 Appendix E" [
    testCase "derive_pubkey" $ do
      let basepoint = Point $ hex
            "036d6caac248af96f6afa7f904f550253a0f3ef3f5aa2fe6838a95b216691468e2"
          perCommitmentPoint = PerCommitmentPoint $ Point $ hex
            "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"
          expected = hex
            "0235f2dbfaa89b57ec7b055afe29849ef7ddfeb1cefdb9ebdc43f5494984db29e5"
      case derive_pubkey basepoint perCommitmentPoint of
        Nothing -> assertFailure "derive_pubkey returned Nothing"
        Just (Pubkey pk) -> pk @?= expected

  , testCase "derive_revocationpubkey" $ do
      let revocationBasepoint = RevocationBasepoint $ Point $ hex
            "036d6caac248af96f6afa7f904f550253a0f3ef3f5aa2fe6838a95b216691468e2"
          perCommitmentPoint = PerCommitmentPoint $ Point $ hex
            "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"
          expected = hex
            "02916e326636d19c33f13e8c0c3a03dd157f332f3e99c317c141dd865eb01f8ff0"
      case derive_revocationpubkey revocationBasepoint perCommitmentPoint of
        Nothing -> assertFailure "derive_revocationpubkey returned Nothing"
        Just (RevocationPubkey (Pubkey pk)) -> pk @?= expected
  ]

-- Secret generation test vectors from Appendix D ------------------------------

secretGenerationTests :: TestTree
secretGenerationTests = testGroup "BOLT #3 Appendix D - Generation" [
    testCase "generate_from_seed 0 final node" $ do
      let seed = hex
            "0000000000000000000000000000000000000000000000000000000000000000"
          i = 281474976710655
          expected = hex
            "02a40c85b6f28da08dfdbe0926c53fab2de6d28c10301f8f7c4073d5e42e3148"
      generate_from_seed seed i @?= expected

  , testCase "generate_from_seed FF final node" $ do
      let seed = hex
            "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
          i = 281474976710655
          expected = hex
            "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc"
      generate_from_seed seed i @?= expected

  , testCase "generate_from_seed FF alternate bits 1" $ do
      let seed = hex
            "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
          i = 0xaaaaaaaaaaa
          expected = hex
            "56f4008fb007ca9acf0e15b054d5c9fd12ee06cea347914ddbaed70d1c13a528"
      generate_from_seed seed i @?= expected

  , testCase "generate_from_seed FF alternate bits 2" $ do
      let seed = hex
            "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
          i = 0x555555555555
          expected = hex
            "9015daaeb06dba4ccc05b91b2f73bd54405f2be9f217fbacd3c5ac2e62327d31"
      generate_from_seed seed i @?= expected

  , testCase "generate_from_seed 01 last nontrivial node" $ do
      let seed = hex
            "0101010101010101010101010101010101010101010101010101010101010101"
          i = 1
          expected = hex
            "915c75942a26bb3a433a8ce2cb0427c29ec6c1775cfc78328b57f6ba7bfeaa9c"
      generate_from_seed seed i @?= expected
  ]

-- Secret storage test vectors from Appendix D ---------------------------------

secretStorageTests :: TestTree
secretStorageTests = testGroup "BOLT #3 Appendix D - Storage" [
    testCase "insert_secret correct sequence" $ do
      let secrets = [
              (281474976710655, hex
                "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc")
            , (281474976710654, hex
                "c7518c8ae4660ed02894df8976fa1a3659c1a8b4b5bec0c4b872abeba4cb8964")
            , (281474976710653, hex
                "2273e227a5b7449b6e70f1fb4652864038b1cbf9cd7c043a7d6456b7fc275ad8")
            , (281474976710652, hex
                "27cddaa5624534cb6cb9d7da077cf2b22ab21e9b506fd4998a51d54502e99116")
            , (281474976710651, hex
                "c65716add7aa98ba7acb236352d665cab17345fe45b55fb879ff80e6bd0c41dd")
            , (281474976710650, hex
                "969660042a28f32d9be17344e09374b379962d03db1574df5a8a5a47e19ce3f2")
            , (281474976710649, hex
                "a5a64476122ca0925fb344bdc1854c1c0a59fc614298e50a33e331980a220f32")
            , (281474976710648, hex
                "05cde6323d949933f7f7b78776bcc1ea6d9b31447732e3802e1f7ac44b650e17")
            ]
      let insertAll store [] = Just store
          insertAll store ((idx, secret):rest) =
            case insert_secret secret idx store of
              Nothing -> Nothing
              Just store' -> insertAll store' rest
      case insertAll empty_store secrets of
        Nothing -> assertFailure "insert_secret failed on correct sequence"
        Just _ -> return ()

  , testCase "insert_secret #1 incorrect" $ do
      -- First secret is from wrong seed, second should fail
      let store0 = empty_store
      case insert_secret (hex
             "02a40c85b6f28da08dfdbe0926c53fab2de6d28c10301f8f7c4073d5e42e3148")
             281474976710655 store0 of
        Nothing -> assertFailure "First insert should succeed"
        Just store1 ->
          case insert_secret (hex
                 "c7518c8ae4660ed02894df8976fa1a3659c1a8b4b5bec0c4b872abeba4cb8964")
                 281474976710654 store1 of
            Nothing -> return ()  -- Expected to fail
            Just _ -> assertFailure "Second insert should fail"
  ]

-- Fee calculation tests -------------------------------------------------------

feeCalculationTests :: TestTree
feeCalculationTests = testGroup "Fee calculation" [
    testCase "commitment_fee no anchors, 0 htlcs" $ do
      let feerate = FeeratePerKw 5000
          features = ChannelFeatures { cf_option_anchors = False }
          fee = commitment_fee feerate features 0
      fee @?= Satoshi 3620  -- 5000 * 724 / 1000 = 3620

  , testCase "commitment_fee no anchors, 2 htlcs" $ do
      let feerate = FeeratePerKw 5000
          features = ChannelFeatures { cf_option_anchors = False }
          fee = commitment_fee feerate features 2
      -- weight = 724 + 172*2 = 1068
      -- fee = 5000 * 1068 / 1000 = 5340
      fee @?= Satoshi 5340

  , testCase "commitment_fee with anchors, 0 htlcs" $ do
      let feerate = FeeratePerKw 5000
          features = ChannelFeatures { cf_option_anchors = True }
          fee = commitment_fee feerate features 0
      -- 5000 * 1124 / 1000 = 5620
      fee @?= Satoshi 5620

  , testCase "htlc_timeout_fee no anchors" $ do
      let feerate = FeeratePerKw 5000
          features = ChannelFeatures { cf_option_anchors = False }
          fee = htlc_timeout_fee feerate features
      -- 5000 * 663 / 1000 = 3315
      fee @?= Satoshi 3315

  , testCase "htlc_success_fee no anchors" $ do
      let feerate = FeeratePerKw 5000
          features = ChannelFeatures { cf_option_anchors = False }
          fee = htlc_success_fee feerate features
      -- 5000 * 703 / 1000 = 3515
      fee @?= Satoshi 3515

  , testCase "htlc_timeout_fee with anchors is 0" $ do
      let feerate = FeeratePerKw 5000
          features = ChannelFeatures { cf_option_anchors = True }
          fee = htlc_timeout_fee feerate features
      fee @?= Satoshi 0

  , testCase "htlc_success_fee with anchors is 0" $ do
      let feerate = FeeratePerKw 5000
          features = ChannelFeatures { cf_option_anchors = True }
          fee = htlc_success_fee feerate features
      fee @?= Satoshi 0
  ]

-- Trimming tests --------------------------------------------------------------

trimmingTests :: TestTree
trimmingTests = testGroup "HTLC trimming" [
    testCase "offered HTLC above threshold not trimmed" $ do
      let dust = DustLimit (Satoshi 546)
          feerate = FeeratePerKw 5000
          features = ChannelFeatures { cf_option_anchors = False }
          htlc = HTLC
            { htlc_direction = HTLCOffered
            , htlc_amount_msat = MilliSatoshi 5000000  -- 5000 sats
            , htlc_payment_hash = PaymentHash (BS.replicate 32 0)
            , htlc_cltv_expiry = CltvExpiry 500000
            }
      -- threshold = 546 + 3315 = 3861
      -- 5000 > 3861, so not trimmed
      is_trimmed dust feerate features htlc @?= False

  , testCase "offered HTLC below threshold is trimmed" $ do
      let dust = DustLimit (Satoshi 546)
          feerate = FeeratePerKw 5000
          features = ChannelFeatures { cf_option_anchors = False }
          htlc = HTLC
            { htlc_direction = HTLCOffered
            , htlc_amount_msat = MilliSatoshi 1000000  -- 1000 sats
            , htlc_payment_hash = PaymentHash (BS.replicate 32 0)
            , htlc_cltv_expiry = CltvExpiry 500000
            }
      -- threshold = 546 + 3315 = 3861
      -- 1000 < 3861, so trimmed
      is_trimmed dust feerate features htlc @?= True

  , testCase "received HTLC above threshold not trimmed" $ do
      let dust = DustLimit (Satoshi 546)
          feerate = FeeratePerKw 5000
          features = ChannelFeatures { cf_option_anchors = False }
          htlc = HTLC
            { htlc_direction = HTLCReceived
            , htlc_amount_msat = MilliSatoshi 7000000  -- 7000 sats
            , htlc_payment_hash = PaymentHash (BS.replicate 32 0)
            , htlc_cltv_expiry = CltvExpiry 500000
            }
      -- threshold = 546 + 3515 = 4061
      -- 7000 > 4061, so not trimmed
      is_trimmed dust feerate features htlc @?= False

  , testCase "received HTLC below threshold is trimmed" $ do
      let dust = DustLimit (Satoshi 546)
          feerate = FeeratePerKw 5000
          features = ChannelFeatures { cf_option_anchors = False }
          htlc = HTLC
            { htlc_direction = HTLCReceived
            , htlc_amount_msat = MilliSatoshi 800000  -- 800 sats
            , htlc_payment_hash = PaymentHash (BS.replicate 32 0)
            , htlc_cltv_expiry = CltvExpiry 500000
            }
      -- threshold = 546 + 3515 = 4061
      -- 800 < 4061, so trimmed
      is_trimmed dust feerate features htlc @?= True
  ]

-- Smart constructor tests -----------------------------------------------------

smartConstructorTests :: TestTree
smartConstructorTests = testGroup "validation" [
    -- 33-byte types
    testCase "pubkey accepts 33 bytes" $ do
      let bs = BS.replicate 33 0x02
      isJust (pubkey bs) @?= True
  , testCase "pubkey rejects 32 bytes" $ do
      let bs = BS.replicate 32 0x02
      isNothing (pubkey bs) @?= True
  , testCase "pubkey rejects 34 bytes" $ do
      let bs = BS.replicate 34 0x02
      isNothing (pubkey bs) @?= True
  , testCase "point accepts 33 bytes" $ do
      let bs = BS.replicate 33 0x03
      isJust (point bs) @?= True
  , testCase "point rejects 32 bytes" $ do
      let bs = BS.replicate 32 0x03
      isNothing (point bs) @?= True

    -- 32-byte types
  , testCase "seckey accepts 32 bytes" $ do
      let bs = BS.replicate 32 0x01
      isJust (seckey bs) @?= True
  , testCase "seckey rejects 31 bytes" $ do
      let bs = BS.replicate 31 0x01
      isNothing (seckey bs) @?= True
  , testCase "seckey rejects 33 bytes" $ do
      let bs = BS.replicate 33 0x01
      isNothing (seckey bs) @?= True
  , testCase "mkTxId accepts 32 bytes" $ do
      let bs = BS.replicate 32 0x00
      isJust (mkTxId bs) @?= True
  , testCase "mkTxId rejects 31 bytes" $ do
      let bs = BS.replicate 31 0x00
      isNothing (mkTxId bs) @?= True
  , testCase "paymentHash accepts 32 bytes" $ do
      let bs = BS.replicate 32 0xab
      isJust (paymentHash bs) @?= True
  , testCase "paymentHash rejects 33 bytes" $ do
      let bs = BS.replicate 33 0xab
      isNothing (paymentHash bs) @?= True
  , testCase "paymentPreimage accepts 32 bytes" $ do
      let bs = BS.replicate 32 0xcd
      isJust (paymentPreimage bs) @?= True
  , testCase "paymentPreimage rejects 31 bytes" $ do
      let bs = BS.replicate 31 0xcd
      isNothing (paymentPreimage bs) @?= True
  , testCase "perCommitmentSecret accepts 32 bytes" $ do
      let bs = BS.replicate 32 0xef
      isJust (perCommitmentSecret bs) @?= True
  , testCase "perCommitmentSecret rejects 33 bytes" $ do
      let bs = BS.replicate 33 0xef
      isNothing (perCommitmentSecret bs) @?= True

    -- 48-bit commitment number
  , testCase "commitment_number accepts 0" $ do
      isJust (commitment_number 0) @?= True
  , testCase "commitment_number accepts 2^48-1" $ do
      isJust (commitment_number 281474976710655) @?= True
  , testCase "commitment_number rejects 2^48" $ do
      isNothing (commitment_number 281474976710656) @?= True
  , testCase "commitment_number rejects maxBound Word64" $ do
      isNothing (commitment_number maxBound) @?= True
  ]
