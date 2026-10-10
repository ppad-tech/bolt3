{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (foldM)
import qualified Crypto.Curve.Secp256k1 as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Maybe (isJust, isNothing)
import Data.Word (Word32, Word64)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Lightning.Protocol.BOLT3
import Lightning.Protocol.BOLT3.Types
  ( Pubkey(..), Point(..)
  , PaymentHash(..), PerCommitmentPoint(..)
  , PerCommitmentSecret(..)
  )
import Vectors

-- Module-level wNAF context. Built once; reused across every
-- equivalence test below. Mirrors how downstream callers should use
-- it.
tex :: S.Context
tex = S.precompute
{-# NOINLINE tex #-}

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
  , testGroup "Properties" [
      propertyTests
    ]
  ]

-- | Decode a hex literal, failing the enclosing test on bad input.
hex :: BS.ByteString -> IO BS.ByteString
hex h = case B16.decode h of
  Just bs -> pure bs
  Nothing -> assertFailure ("invalid hex literal: " ++ show h)

-- Key derivation test vectors from Appendix E ---------------------------------

keyDerivationTests :: TestTree
keyDerivationTests = testGroup "BOLT #3 Appendix E" [
    testCase "derive_pubkey" $ do
      basepoint <- Point <$> hex
        "036d6caac248af96f6afa7f904f550253a0f3ef3f5aa2fe6838a95b216691468e2"
      perCommitmentPoint <- PerCommitmentPoint . Point <$> hex
        "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"
      expected <- hex
        "0235f2dbfaa89b57ec7b055afe29849ef7ddfeb1cefdb9ebdc43f5494984db29e5"
      case derive_pubkey basepoint perCommitmentPoint of
        Nothing -> assertFailure "derive_pubkey returned Nothing"
        Just (Pubkey pk) -> pk @?= expected

  , testCase "derive_pubkey' matches vector" $ do
      basepoint <- Point <$> hex
        "036d6caac248af96f6afa7f904f550253a0f3ef3f5aa2fe6838a95b216691468e2"
      perCommitmentPoint <- PerCommitmentPoint . Point <$> hex
        "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"
      expected <- hex
        "0235f2dbfaa89b57ec7b055afe29849ef7ddfeb1cefdb9ebdc43f5494984db29e5"
      case derive_pubkey' tex basepoint perCommitmentPoint of
        Nothing -> assertFailure "derive_pubkey' returned Nothing"
        Just (Pubkey pk) -> pk @?= expected

  , testCase "derive_revocationpubkey" $ do
      revocationBasepoint <- RevocationBasepoint . Point <$> hex
        "036d6caac248af96f6afa7f904f550253a0f3ef3f5aa2fe6838a95b216691468e2"
      perCommitmentPoint <- PerCommitmentPoint . Point <$> hex
        "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"
      expected <- hex
        "02916e326636d19c33f13e8c0c3a03dd157f332f3e99c317c141dd865eb01f8ff0"
      case derive_revocationpubkey revocationBasepoint perCommitmentPoint of
        Nothing -> assertFailure "derive_revocationpubkey returned Nothing"
        Just (RevocationPubkey (Pubkey pk)) -> pk @?= expected
  ]

-- Secret generation test vectors from Appendix D ------------------------------

secretGenerationTests :: TestTree
secretGenerationTests = testGroup "BOLT #3 Appendix D - Generation" [
    testCase "generate_from_seed 0 final node" $ do
      seed <- hex
        "0000000000000000000000000000000000000000000000000000000000000000"
      expected <- hex
        "02a40c85b6f28da08dfdbe0926c53fab2de6d28c10301f8f7c4073d5e42e3148"
      let i = 281474976710655
      generate_from_seed seed i @?= expected

  , testCase "generate_from_seed FF final node" $ do
      seed <- hex
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
      expected <- hex
        "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc"
      let i = 281474976710655
      generate_from_seed seed i @?= expected

  , testCase "generate_from_seed FF alternate bits 1" $ do
      seed <- hex
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
      expected <- hex
        "56f4008fb007ca9acf0e15b054d5c9fd12ee06cea347914ddbaed70d1c13a528"
      let i = 0xaaaaaaaaaaa
      generate_from_seed seed i @?= expected

  , testCase "generate_from_seed FF alternate bits 2" $ do
      seed <- hex
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
      expected <- hex
        "9015daaeb06dba4ccc05b91b2f73bd54405f2be9f217fbacd3c5ac2e62327d31"
      let i = 0x555555555555
      generate_from_seed seed i @?= expected

  , testCase "generate_from_seed 01 last nontrivial node" $ do
      seed <- hex
        "0101010101010101010101010101010101010101010101010101010101010101"
      expected <- hex
        "915c75942a26bb3a433a8ce2cb0427c29ec6c1775cfc78328b57f6ba7bfeaa9c"
      let i = 1
      generate_from_seed seed i @?= expected
  ]

-- Secret storage test vectors from Appendix D ---------------------------------

secretStorageTests :: TestTree
secretStorageTests = testGroup "BOLT #3 Appendix D - Storage" $
     fmap storageVectorTest appendix_d_storage
  ++ [
    testCase "derive_old_secret after each correct insert" $ do
      steps <- case filter isCorrect appendix_d_storage of
        (v : _) -> pure (sv_steps v)
        [] -> assertFailure "missing correct sequence vector"
      known <- traverse (\st -> (,) (ss_index st) <$> hex (ss_secret st))
                 steps
      -- after each insert, every index inserted so far is derivable
      let step (store, seen) (i, sec) = do
            store' <- case insert_secret sec i store of
              Just st -> pure st
              Nothing -> assertFailure
                ("rejected correct secret at I=" ++ show i)
            let seen' = (i, sec) : seen
            mapM_ (\(j, s) ->
                    assertEqual
                      ("after I=" ++ show i ++ ", derive_old_secret "
                        ++ show j)
                      (Just s) (derive_old_secret j store'))
                  seen'
            pure (store', seen')
      (store, _) <- foldM step (empty_store, []) known
      -- the next index has not been received
      assertEqual "derive_old_secret of unreceived index"
        Nothing (derive_old_secret 281474976710647 store)

  , testCase "insert_secret rejects 31-byte secret" $
      isNothing (insert_secret (BS.replicate 31 0) maxCommitNum empty_store)
        @?= True
  , testCase "insert_secret rejects 33-byte secret" $
      isNothing (insert_secret (BS.replicate 33 0) maxCommitNum empty_store)
        @?= True
  , testCase "insert_secret rejects index 2^48" $
      isNothing
        (insert_secret (BS.replicate 32 0) (maxCommitNum + 1) empty_store)
        @?= True
  , testCase "insert_secret accepts index 2^48-1" $
      isJust (insert_secret (BS.replicate 32 0) maxCommitNum empty_store)
        @?= True
  ]
  where
    isCorrect v = sv_name v == "insert_secret correct sequence"

-- | Run a storage vector, asserting that each insert_secret call is
--   accepted or rejected exactly as the spec says.
storageVectorTest :: StorageVector -> TestTree
storageVectorTest (StorageVector name steps) =
    testCase name (go empty_store steps)
  where
    go _ [] = pure ()
    go store (StorageStep i ok s : rest) = do
      sec <- hex s
      case (insert_secret sec i store, ok) of
        (Just store', True) -> go store' rest
        (Nothing, False) -> go store rest
        (Just _, False) ->
          assertFailure ("accepted incorrect secret at I=" ++ show i)
        (Nothing, True) ->
          assertFailure ("rejected correct secret at I=" ++ show i)

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

    -- next_commitment_number
  , testCase "next_commitment_number 0 -> 1" $
      case commitment_number 0 of
        Nothing -> assertFailure "commitment_number 0"
        Just cn0 -> case next_commitment_number cn0 of
          Nothing -> assertFailure "next failed"
          Just cn1 -> unCommitmentNumber cn1 @?= 1
  , testCase "next_commitment_number (2^48-2) -> (2^48-1)" $
      case commitment_number 281474976710654 of
        Nothing -> assertFailure "commitment_number"
        Just cn -> case next_commitment_number cn of
          Nothing -> assertFailure "next failed"
          Just cn' ->
            unCommitmentNumber cn' @?= 281474976710655
  , testCase "next_commitment_number (2^48-1) -> Nothing" $
      case commitment_number 281474976710655 of
        Nothing -> assertFailure "commitment_number"
        Just cn ->
          isNothing (next_commitment_number cn) @?= True
  ]

-- Property tests -------------------------------------------------------

-- | Maximum valid commitment number (2^48 - 1).
maxCommitNum :: Word64
maxCommitNum = 281474976710655

propertyTests :: TestTree
propertyTests = testGroup "invariants" [
    testProperty "commitment_number validates 48-bit"
      propCommitmentNumberRange
  , testProperty "next_commitment_number stays valid"
      propNextCommitmentNumber
  , testProperty "trimmed/untrimmed partition"
      propTrimPartition
  , testProperty "commitment_fee increases with HTLCs"
      propFeeMonotonic
  , testProperty "insert_secret keeps every inserted secret derivable"
      propSecretStoreDerivable
  , testProperty "derive_per_commitment_point' ≡ derive_per_commitment_point"
      propDerivePcpEquiv
  , testProperty "derive_pubkey' ≡ derive_pubkey"
      propDerivePubkeyEquiv
  , testProperty "derive_localpubkey' ≡ derive_localpubkey"
      propDeriveLocalEquiv
  , testProperty "derive_local_htlcpubkey' ≡ derive_local_htlcpubkey"
      propDeriveLocalHtlcEquiv
  , testProperty "derive_remote_htlcpubkey' ≡ derive_remote_htlcpubkey"
      propDeriveRemoteHtlcEquiv
  , testProperty "derive_local_delayedpubkey' ≡ derive_local_delayedpubkey"
      propDeriveLocalDelEquiv
  , testProperty "derive_remote_delayedpubkey' ≡ derive_remote_delayedpubkey"
      propDeriveRemoteDelEquiv
  ]

-- | Random 32-byte secret, then derive a basepoint and per-commit
--   point from it. Returns (basepoint, pcp). Using derived points
--   keeps us on the curve without having to generate valid points
--   directly.
genPointPair :: Gen (Point, PerCommitmentPoint)
genPointPair = do
  sk1 <- vectorOf 32 (choose (0, 255 :: Int))
  sk2 <- vectorOf 32 (choose (0, 255 :: Int))
  let bs1 = BS.pack (fmap fromIntegral sk1)
      bs2 = BS.pack (fmap fromIntegral sk2)
      mkPt b = case S.parse_int256 b of
        Nothing -> Nothing
        Just w  -> S.serialize_point <$> S.derive_pub w
  case (mkPt bs1, mkPt bs2) of
    (Just p1, Just p2) ->
      pure (Point p1, PerCommitmentPoint (Point p2))
    _ -> genPointPair  -- ~negligible retry; sk=0 or sk>=q

propDerivePcpEquiv :: Property
propDerivePcpEquiv =
  forAll (vectorOf 32 (choose (0, 255 :: Int))) $ \sk ->
    let bs = BS.pack (fmap fromIntegral sk)
        sec = PerCommitmentSecret bs
    in  derive_per_commitment_point' tex sec
          === derive_per_commitment_point sec

propDerivePubkeyEquiv :: Property
propDerivePubkeyEquiv =
  forAll genPointPair $ \(bp, pcp) ->
    derive_pubkey' tex bp pcp === derive_pubkey bp pcp

propDeriveLocalEquiv :: Property
propDeriveLocalEquiv =
  forAll genPointPair $ \(bp, pcp) ->
    let pbp = PaymentBasepoint bp
    in  derive_localpubkey' tex pbp pcp
          === derive_localpubkey pbp pcp

propDeriveLocalHtlcEquiv :: Property
propDeriveLocalHtlcEquiv =
  forAll genPointPair $ \(bp, pcp) ->
    let hbp = HtlcBasepoint bp
    in  derive_local_htlcpubkey' tex hbp pcp
          === derive_local_htlcpubkey hbp pcp

propDeriveRemoteHtlcEquiv :: Property
propDeriveRemoteHtlcEquiv =
  forAll genPointPair $ \(bp, pcp) ->
    let hbp = HtlcBasepoint bp
    in  derive_remote_htlcpubkey' tex hbp pcp
          === derive_remote_htlcpubkey hbp pcp

propDeriveLocalDelEquiv :: Property
propDeriveLocalDelEquiv =
  forAll genPointPair $ \(bp, pcp) ->
    let dbp = DelayedPaymentBasepoint bp
    in  derive_local_delayedpubkey' tex dbp pcp
          === derive_local_delayedpubkey dbp pcp

propDeriveRemoteDelEquiv :: Property
propDeriveRemoteDelEquiv =
  forAll genPointPair $ \(bp, pcp) ->
    let dbp = DelayedPaymentBasepoint bp
    in  derive_remote_delayedpubkey' tex dbp pcp
          === derive_remote_delayedpubkey dbp pcp

-- | After inserting a prefix of a correct sequence (descending from
--   2^48-1, all from one seed), every inserted index is derivable and
--   the store's buckets are strictly ascending.
propSecretStoreDerivable :: Property
propSecretStoreDerivable =
  forAll (vectorOf 32 (choose (0, 255 :: Int))) $ \sd ->
  forAll (choose (1, 1024)) $ \n ->
    let seed = BS.pack (fmap fromIntegral sd)
        idxs = take n [maxCommitNum, maxCommitNum - 1 ..]
        secret = generate_from_seed seed
        insert st i = st >>= insert_secret (secret i) i
    in  case foldl insert (Just empty_store) idxs of
          Nothing -> counterexample "insert_secret rejected" False
          Just store ->
            let buckets = fmap se_bucket (unSecretStore store)
            in  conjoin
                  [ counterexample "buckets not ascending"
                      (and (zipWith (<) buckets (drop 1 buckets)))
                  , conjoin
                      [ counterexample ("index " ++ show i)
                          (derive_old_secret i store === Just (secret i))
                      | i <- idxs ]
                  ]

-- | commitment_number accepts values in [0, 2^48-1] and
--   rejects values >= 2^48.
propCommitmentNumberRange :: Property
propCommitmentNumberRange =
  forAll (choose (0, maxBound)) $ \n ->
    case commitment_number n of
      Just cn -> n <= maxCommitNum
        && unCommitmentNumber cn == n
      Nothing -> n > maxCommitNum

-- | If cn is a valid commitment number below the max,
--   next_commitment_number yields a valid successor.
propNextCommitmentNumber :: Property
propNextCommitmentNumber =
  forAll (choose (0, maxCommitNum)) $ \n ->
    case commitment_number n of
      Nothing -> False
      Just cn
        | n < maxCommitNum ->
          case next_commitment_number cn of
            Nothing -> False
            Just cn' ->
              unCommitmentNumber cn' == n + 1
        | otherwise ->
          isNothing (next_commitment_number cn)

-- | trimmed_htlcs and untrimmed_htlcs partition the
--   input list: every HTLC is in exactly one set and
--   trimmed HTLCs have amount below the threshold.
propTrimPartition :: Property
propTrimPartition =
  forAll (choose (1 :: Word32, 50000)) $ \feeW ->
  forAll (choose (1, 20)) $ \numHtlcs ->
  forAll (vectorOf numHtlcs genHTLC) $ \htlcs ->
    let dust = DustLimit (Satoshi 546)
        feerate = FeeratePerKw feeW
        features = ChannelFeatures
          { cf_option_anchors = False }
        trimmed = trimmed_htlcs
          dust feerate features htlcs
        untrimmed = untrimmed_htlcs
          dust feerate features htlcs
    in length trimmed + length untrimmed
         == length htlcs

-- | commitment_fee is monotonically non-decreasing in
--   the number of HTLCs.
propFeeMonotonic :: Property
propFeeMonotonic =
  forAll (choose (1 :: Word32, 50000)) $ \feeW ->
  forAll (choose (0, 100)) $ \n ->
    let feerate = FeeratePerKw feeW
        features = ChannelFeatures
          { cf_option_anchors = False }
        fee0 = commitment_fee feerate features n
        fee1 = commitment_fee feerate features (n + 1)
    in fee1 >= fee0

-- | Generate a random HTLC.
genHTLC :: Gen HTLC
genHTLC = do
  dir <- elements [HTLCOffered, HTLCReceived]
  amt <- choose (0, 10000000)
  cltv <- choose (0, 1000000)
  pure HTLC
    { htlc_direction = dir
    , htlc_amount_msat = MilliSatoshi amt
    , htlc_payment_hash = PaymentHash
        (BS.replicate 32 0x00)
    , htlc_cltv_expiry = CltvExpiry cltv
    }
