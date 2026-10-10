{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Bitcoin.Prim.Tx as BT
import qualified Bitcoin.Prim.Tx.Sighash as Sighash
import Control.Monad (foldM, forM_)
import qualified Crypto.Curve.Secp256k1 as S
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bits (xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, isNothing)
import Data.Word (Word8, Word64)
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT3
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Vectors

-- Shared wNAF context.
tex :: S.Context
tex = S.precompute
{-# NOINLINE tex #-}

main :: IO ()
main = defaultMain $ testGroup "ppad-bolt3" [
    keyTests
  , secretGenerationTests
  , secretStorageTests
  , fundingTests
  , testGroup "Commitment and HTLC transactions (Appendix C)" $
      fmap (commitVectorTests StaticRemotekey) appendix_c
  , testGroup "Commitment and HTLC transactions (Appendix F)" $
      fmap (commitVectorTests Anchors) appendix_f
  , commitmentTests
  , htlcTxTests
  , closingTests
  , scriptTests
  , feeTests
  , constructorTests
  , propertyTests
  ]

-- helpers --------------------------------------------------------------------

-- | Decode a hex literal, failing the enclosing test on bad input.
hex :: BS.ByteString -> IO BS.ByteString
hex h = case B16.decode h of
  Just bs -> pure bs
  Nothing -> assertFailure ("invalid hex literal: " ++ show h)

-- | Unwrap a Just, failing the enclosing test on Nothing.
need :: String -> Maybe a -> IO a
need msg = maybe (assertFailure msg) pure

pt :: BS.ByteString -> IO BOLT1.Point
pt h = hex h >>= need ("invalid point " ++ show h) . BOLT1.point

sat :: Word64 -> IO BOLT1.Satoshi
sat = need "invalid satoshi amount" . BOLT1.satoshi

msat :: Word64 -> IO BOLT1.MilliSatoshi
msat = need "invalid millisatoshi amount" . BOLT1.milli_satoshi

sk :: BS.ByteString -> IO Seckey
sk h = hex h >>= need ("invalid secret key " ++ show h) . seckey

pcsecret :: BS.ByteString -> IO BOLT1.PerCommitmentSecret
pcsecret h = hex h >>= need "invalid secret" . BOLT1.per_commitment_secret

idx :: Word64 -> IO SecretIndex
idx = need "invalid secret index" . secret_index

unpoint :: BOLT1.Point -> BS.ByteString
unpoint = BOLT1.un_point

unsecret :: BOLT1.PerCommitmentSecret -> BS.ByteString
unsecret = BOLT1.un_per_commitment_secret

-- | The compressed public key of a secret key.
pubkey_of :: Seckey -> Maybe BS.ByteString
pubkey_of k = do
  w <- S.parse_int256 (un_seckey k)
  S.serialize_point <$> S.derive_pub w

-- Appendix E, and the internal values of Appendix C --------------------------

e_base_secret, e_per_commitment_secret, e_base_point,
  e_per_commitment_point :: BS.ByteString
e_base_secret =
  "000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f"
e_per_commitment_secret =
  "1f1e1d1c1b1a191817161514131211100f0e0d0c0b0a09080706050403020100"
e_base_point =
  "036d6caac248af96f6afa7f904f550253a0f3ef3f5aa2fe6838a95b216691468e2"
e_per_commitment_point =
  "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"

-- | Appendix C's (commented) derivation inputs.
c_local_delayed_payment_basepoint_secret, c_local_delayed_payment_basepoint,
  c_remote_revocation_basepoint, c_local_per_commitment_point,
  c_local_delayed_privkey :: BS.ByteString
c_local_delayed_payment_basepoint_secret =
  "3333333333333333333333333333333333333333333333333333333333333333"
c_local_delayed_payment_basepoint =
  "023c72addb4fdf09af94f0c94d7fe92a386a7e70cf8a1d85916386bb2535c7b1b1"
c_remote_revocation_basepoint =
  "02466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f27"
c_local_per_commitment_point =
  "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"
c_local_delayed_privkey =
  "adf3464ce9c2f230fd2582fda4c6965e4993ca5524e8c9580e3df0cf226981ad"

keyTests :: TestTree
keyTests = testGroup "Key derivation (Appendix E)" [
    testCase "per_commitment_point from per_commitment_secret" $ do
      s <- pcsecret e_per_commitment_secret
      expected <- hex e_per_commitment_point
      PerCommitmentPoint p <- need "derive_per_commitment_point"
        (derive_per_commitment_point s)
      unpoint p @?= expected
      PerCommitmentPoint p' <- need "derive_per_commitment_point'"
        (derive_per_commitment_point' tex s)
      unpoint p' @?= expected

  , testCase "localpubkey from basepoint and per_commitment_point" $ do
      bp <- pt e_base_point
      pcp <- PerCommitmentPoint <$> pt e_per_commitment_point
      expected <- hex
        "0235f2dbfaa89b57ec7b055afe29849ef7ddfeb1cefdb9ebdc43f5494984db29e5"
      fmap unpoint (derive_pubkey bp pcp) @?= Just expected

  , testCase "localprivkey from basepoint secret" $ do
      bs <- sk e_base_secret
      pcp <- PerCommitmentPoint <$> pt e_per_commitment_point
      expected <- hex
        "cbced912d3b21bf196a766651e436aff192362621ce317704ea2f75d87e7be0f"
      fmap un_seckey (derive_privkey bs pcp) @?= Just expected

  , testCase "revocationpubkey from basepoint and per_commitment_point" $ do
      rbp <- RevocationBasepoint <$> pt e_base_point
      pcp <- PerCommitmentPoint <$> pt e_per_commitment_point
      expected <- hex
        "02916e326636d19c33f13e8c0c3a03dd157f332f3e99c317c141dd865eb01f8ff0"
      fmap (\(RevocationPubkey p) -> unpoint p)
        (derive_revocationpubkey rbp pcp) @?= Just expected

  , testCase "revocationprivkey from secrets" $ do
      rbs <- sk e_base_secret
      pcs <- pcsecret e_per_commitment_secret
      expected <- hex
        "d09ffff62ddb2297ab000cc85bcb4283fdeb6aa052affbc9dddcf33b61078110"
      fmap un_seckey (derive_revocationprivkey rbs pcs) @?= Just expected

  , testCase "Appendix C local_delayed_privkey" $ do
      bs <- sk c_local_delayed_payment_basepoint_secret
      pcp <- PerCommitmentPoint <$> pt c_local_per_commitment_point
      expected <- hex c_local_delayed_privkey
      fmap un_seckey (derive_privkey bs pcp) @?= Just expected
      -- whose public key is Appendix C's local_delayedpubkey
      expected_pub <- hex c_local_delayedpubkey
      (pubkey_of =<< derive_privkey bs pcp) @?= Just expected_pub

  , testCase "Appendix C commitment keys from basepoints" $ do
      keys <- vectorKeys
      pcp <- PerCommitmentPoint <$> pt c_local_per_commitment_point
      local_bp <- pt c_local_payment_basepoint
      remote_bp <- pt c_remote_payment_basepoint
      local_delayed_bp <- pt c_local_delayed_payment_basepoint
      remote_rev_bp <- pt c_remote_revocation_basepoint
      let local = Basepoints
            { bp_revocation      = RevocationBasepoint local_bp
            , bp_payment         = PaymentBasepoint local_bp
            , bp_delayed_payment =
                DelayedPaymentBasepoint local_delayed_bp
            , bp_htlc            = HtlcBasepoint local_bp
            }
          remote = Basepoints
            { bp_revocation      = RevocationBasepoint remote_rev_bp
            , bp_payment         = PaymentBasepoint remote_bp
            , bp_delayed_payment = DelayedPaymentBasepoint remote_bp
            , bp_htlc            = HtlcBasepoint remote_bp
            }
          derived = derive_commitment_keys local (ck_local_funding keys)
            remote (ck_remote_funding keys) pcp
      derived @?= Just keys
      derive_commitment_keys' tex local (ck_local_funding keys)
        remote (ck_remote_funding keys) pcp @?= Just keys

  , testCase "rejects a basepoint off the curve" $ do
      bad <- pt
        "020000000000000000000000000000000000000000000000000000000000000000"
      pcp <- PerCommitmentPoint <$> pt e_per_commitment_point
      isNothing (derive_pubkey bad pcp) @?= True
      isNothing (derive_revocationpubkey (RevocationBasepoint bad) pcp)
        @?= True

  , testCase "rejects a per_commitment_point off the curve" $ do
      rbp <- RevocationBasepoint <$> pt e_base_point
      bad <- PerCommitmentPoint <$> pt
        "020000000000000000000000000000000000000000000000000000000000000000"
      isNothing (derive_revocationpubkey rbp bad) @?= True

  , testCase "rejects an invalid per_commitment_secret" $ do
      zero <- pcsecret
        "0000000000000000000000000000000000000000000000000000000000000000"
      big <- pcsecret
        "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
      rbs <- sk e_base_secret
      isNothing (derive_per_commitment_point zero) @?= True
      isNothing (derive_per_commitment_point big) @?= True
      isNothing (derive_per_commitment_point' tex big) @?= True
      isNothing (derive_revocationprivkey rbs zero) @?= True
      isNothing (derive_revocationprivkey rbs big) @?= True
  ]

-- Appendix D: generation -----------------------------------------------------

secretGenerationTests :: TestTree
secretGenerationTests = testGroup "Secret generation (Appendix D)" [
    gen "generate_from_seed 0 final node"
      "0000000000000000000000000000000000000000000000000000000000000000"
      281474976710655
      "02a40c85b6f28da08dfdbe0926c53fab2de6d28c10301f8f7c4073d5e42e3148"
  , gen "generate_from_seed FF final node"
      "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
      281474976710655
      "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc"
  , gen "generate_from_seed FF alternate bits 1"
      "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
      0xaaaaaaaaaaa
      "56f4008fb007ca9acf0e15b054d5c9fd12ee06cea347914ddbaed70d1c13a528"
  , gen "generate_from_seed FF alternate bits 2"
      "ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
      0x555555555555
      "9015daaeb06dba4ccc05b91b2f73bd54405f2be9f217fbacd3c5ac2e62327d31"
  , gen "generate_from_seed 01 last nontrivial node"
      "0101010101010101010101010101010101010101010101010101010101010101"
      1
      "915c75942a26bb3a433a8ce2cb0427c29ec6c1775cfc78328b57f6ba7bfeaa9c"
  , testCase "commitment_secret_index" $ do
      cn <- need "commitment_number" (commitment_number 42)
      un_secret_index (commitment_secret_index cn) @?= 0xFFFFFFFFFFFF - 42
  ]
  where
    gen name s i expected = testCase name $ do
      sd <- hex s >>= need "seed" . seed
      ix <- idx i
      e <- hex expected
      unsecret (generate_from_seed sd ix) @?= e

-- Appendix D: storage --------------------------------------------------------

secretStorageTests :: TestTree
secretStorageTests = testGroup "Secret storage (Appendix D)" $
     fmap storageVectorTest appendix_d_storage
  ++ [
    testCase "derive_old_secret after each correct insert" $ do
      known <- correctSequence
      let step (store, seen) (i, s) = do
            store' <- need ("rejected correct secret at I=" ++ show i)
                        (insert_secret s i store)
            let seen' = (i, s) : seen
            forM_ seen' $ \(j, t) ->
              assertEqual
                ("after I=" ++ show i ++ ", derive_old_secret " ++ show j)
                (Just (unsecret t))
                (fmap unsecret (derive_old_secret j store'))
            pure (store', seen')
      (store, _) <- foldM step (empty_secret_store, []) known
      -- the next index has not been received
      next <- idx 281474976710647
      isNothing (derive_old_secret next store) @?= True

  , testCase "un_secret_store/secret_store round trip" $ do
      known <- correctSequence
      store <- need "insert" (foldM (\st (i, s) -> insert_secret s i st)
                                    empty_secret_store known)
      store' <- need "secret_store" (secret_store (un_secret_store store))
      forM_ known $ \(i, s) ->
        fmap unsecret (derive_old_secret i store') @?= Just (unsecret s)
      -- entry order doesn't matter
      store'' <- need "secret_store (reversed)"
        (secret_store (reverse (un_secret_store store)))
      forM_ known $ \(i, s) ->
        fmap unsecret (derive_old_secret i store'') @?= Just (unsecret s)

  , testCase "secret_store rejects a repeated bucket" $ do
      known <- correctSequence
      case known of
        ((i0, s0) : _ : (i2, s2) : _) ->
          -- indices 2^48-1 and 2^48-3 both have no trailing zeros
          isNothing (secret_store [(i0, s0), (i2, s2)]) @?= True
        _ -> assertFailure "short sequence"

  , testCase "secret_store rejects contradictory entries" $ do
      known <- correctSequence
      bad <- pcsecret
        "0000000000000000000000000000000000000000000000000000000000000000"
      case known of
        ((i0, _) : (i1, s1) : _) ->
          -- index 2^48-1 is derivable from index 2^48-2
          isNothing (secret_store [(i0, bad), (i1, s1)]) @?= True
        _ -> assertFailure "short sequence"

  , testCase "Show is redacted" $ do
      known <- correctSequence
      store <- need "insert" (foldM (\st (i, s) -> insert_secret s i st)
                                    empty_secret_store known)
      show store @?= "SecretStore <redacted>"
      sd <- need "seed" (seed (BS.replicate 32 0xff))
      show sd @?= "Seed <redacted>"
      k <- sk e_base_secret
      show k @?= "Seckey <redacted>"
  ]

-- | The spec's correct storage sequence, as (index, secret) pairs.
correctSequence :: IO [(SecretIndex, BOLT1.PerCommitmentSecret)]
correctSequence = do
  steps <- case filter isCorrect appendix_d_storage of
    (v : _) -> pure (sv_steps v)
    [] -> assertFailure "missing correct sequence vector"
  traverse (\st -> (,) <$> idx (ss_index st) <*> pcsecret (ss_secret st))
    steps
  where
    isCorrect v = sv_name v == "insert_secret correct sequence"

-- | Run a storage vector, asserting that each insert_secret call is
--   accepted or rejected exactly as the spec says.
storageVectorTest :: StorageVector -> TestTree
storageVectorTest (StorageVector name steps) =
    testCase name (go empty_secret_store steps)
  where
    go _ [] = pure ()
    go store (StorageStep i ok s : rest) = do
      sec <- pcsecret s
      ix <- idx i
      case (insert_secret sec ix store, ok) of
        (Just store', True) -> go store' rest
        (Nothing, False) -> go store rest
        (Just _, False) ->
          assertFailure ("accepted incorrect secret at I=" ++ show i)
        (Nothing, True) ->
          assertFailure ("rejected correct secret at I=" ++ show i)

-- Appendix B -----------------------------------------------------------------

fundingTests :: TestTree
fundingTests = testGroup "Funding transaction (Appendix B)" [
    testCase "funding witness script" $ do
      (lpk, rpk) <- fundingPubkeys
      wscript <- hex b_funding_wscript
      funding_script lpk rpk @?= Script wscript
      funding_script rpk lpk @?= Script wscript

  , testCase "funding output" $ do
      (lpk, rpk) <- fundingPubkeys
      tx <- hex b_funding_tx >>= decodeTx
      txid <- hex b_funding_txid
      BT.txid tx @?= BT.TxId (BS.reverse txid)
      case drop (fromIntegral b_funding_output) (NE.toList (BT.tx_outputs tx))
        of
          (BT.TxOut value spk : _) -> do
            value @?= b_funding_satoshis
            Script spk @?= to_p2wsh (funding_script lpk rpk)
          [] -> assertFailure "missing funding output"
  ]
  where
    fundingPubkeys = do
      lpk <- FundingPubkey <$> pt b_local_funding_pubkey
      rpk <- FundingPubkey <$> pt b_remote_funding_pubkey
      pure (lpk, rpk)

-- Appendices C and F ---------------------------------------------------------

-- | Decode a serialized transaction, failing the test on bad input.
decodeTx :: BS.ByteString -> IO BT.Tx
decodeTx = need "undecodable transaction" . BT.from_bytes

-- | The single input's witness stack of a spec transaction.
witnessOf :: BT.Tx -> IO [BS.ByteString]
witnessOf tx = case BT.tx_witnesses tx of
  [BT.Witness items] -> pure items
  _ -> assertFailure "expected one witness"

-- | A test HTLC from the Appendix C parameters.
testHtlc :: Int -> IO HTLC
testHtlc i = case drop i c_htlcs of
  (TestHtlc offered amt expiry pre : _) -> do
    preimage <- hex pre
    amount <- msat amt
    ph <- need "payment hash" (BOLT1.payment_hash (SHA256.hash preimage))
    pure HTLC
      { htlc_direction    = if offered then HTLCOffered else HTLCReceived
      , htlc_amount_msat  = amount
      , htlc_payment_hash = ph
      , htlc_cltv_expiry  = CltvExpiry expiry
      }
  [] -> assertFailure ("no test HTLC #" ++ show i)

testPreimage :: Int -> IO BOLT1.PaymentPreimage
testPreimage i = case drop i c_htlcs of
  (TestHtlc _ _ _ pre : _) ->
    hex pre >>= need "preimage" . BOLT1.payment_preimage
  [] -> assertFailure ("no test HTLC #" ++ show i)

-- | Commitment keys from the Appendix C parameters, which Appendix F
--   shares. The vectors use the payment basepoints as HTLC
--   basepoints; to_remote pays the remote payment basepoint.
vectorKeys :: IO CommitmentKeys
vectorKeys = do
  revocation <- pt c_local_revocation_pubkey
  delayed <- pt c_local_delayedpubkey
  local_htlc <- pt c_local_htlcpubkey
  remote_htlc <- pt c_remote_htlcpubkey
  remote_payment <- pt c_remote_payment_basepoint
  local_funding <- pt c_local_funding_pubkey
  remote_funding <- pt c_remote_funding_pubkey
  pure CommitmentKeys
    { ck_revocation_pubkey = RevocationPubkey revocation
    , ck_local_delayed     = LocalDelayedPubkey delayed
    , ck_local_htlc        = LocalHtlcPubkey local_htlc
    , ck_remote_htlc       = RemoteHtlcPubkey remote_htlc
    , ck_remote_payment    = RemotePubkey remote_payment
    , ck_local_funding     = FundingPubkey local_funding
    , ck_remote_funding    = FundingPubkey remote_funding
    }

vectorOutpoint :: IO BT.OutPoint
vectorOutpoint = do
  txid <- hex c_funding_txid
  pure (BT.OutPoint (BT.TxId (BS.reverse txid)) c_funding_output_index)

-- | The commitment context for a vector: local's commitment, with local
--   as the opener.
vectorContext :: CommitmentFormat -> CommitVector -> IO CommitmentContext
vectorContext fmt v = do
  keys <- vectorKeys
  outpoint <- vectorOutpoint
  local_bp <- PaymentBasepoint <$> pt c_local_payment_basepoint
  remote_bp <- PaymentBasepoint <$> pt c_remote_payment_basepoint
  cn <- need "commitment number" (commitment_number c_commitment_number)
  htlcs <- traverse testHtlc (cv_htlcs v)
  dust <- DustLimit <$> sat (cv_dust_limit_sat v)
  to_local <- msat (cv_to_local_msat v)
  to_remote <- msat (cv_to_remote_msat v)
  pure CommitmentContext
    { cc_funding_outpoint  = outpoint
    , cc_commitment_number = cn
    , cc_local_payment_bp  = local_bp
    , cc_remote_payment_bp = remote_bp
    , cc_to_self_delay     = ToSelfDelay c_local_delay
    , cc_dust_limit        = dust
    , cc_feerate           = FeeratePerKw (cv_feerate_per_kw v)
    , cc_format            = fmt
    , cc_is_funder         = True
    , cc_to_local_msat     = to_local
    , cc_to_remote_msat    = to_remote
    , cc_htlcs             = htlcs
    , cc_keys              = keys
    }

-- | Assert that the commitment tx and each HTLC tx of a vector match
--   the spec's, with witnesses stripped, and that the spec's witnesses
--   are those the witness functions build.
commitVectorTests :: CommitmentFormat -> CommitVector -> TestTree
commitVectorTests fmt v = testGroup (cv_name v) $
    testCase "commitment tx" (do
      ctx <- vectorContext fmt v
      spec <- hex (cv_commit_tx v) >>= decodeTx
      commit <- need "no commitment tx" (build_commitment_tx ctx)
      encode_commitment_tx commit @?= BT.to_bytes_legacy spec
      -- the funding witness, with its signatures in either order
      w <- witnessOf spec
      case w of
        [_, s1, s2, _] -> do
          let keys = cc_keys ctx
              lpk = ck_local_funding keys
              rpk = ck_remote_funding keys
          funding_witness lpk s1 rpk s2 @?= BT.Witness w
          funding_witness rpk s2 lpk s1 @?= BT.Witness w
        _ -> assertFailure "unexpected funding witness")
  : fmap htlcTxTest (cv_htlc_txs v)
  where
    htlcTxTest (HtlcTxVector kind i out raw) =
      testCase (kindName kind ++ " for htlc #" ++ show i
                 ++ " (output " ++ show out ++ ")") $ do
        ctx <- vectorContext fmt v
        commit <- need "no commitment tx" (build_commitment_tx ctx)
        expected_htlc <- testHtlc i
        -- the output names its HTLC
        htlc <- case drop (fromIntegral out) (NE.toList (ctx_outputs commit))
          of
            (CommitmentOutput _ _ (OutputHTLC h) : _) -> pure h
            _ -> assertFailure "not an HTLC output"
        htlc @?= expected_htlc
        let keys = cc_keys ctx
            hctx = HTLCContext
              { hc_commitment_txid   = BT.txid (commitment_to_tx commit)
              , hc_output_index      = out
              , hc_htlc              = htlc
              , hc_to_self_delay     = cc_to_self_delay ctx
              , hc_feerate           = cc_feerate ctx
              , hc_format            = fmt
              , hc_revocation_pubkey = ck_revocation_pubkey keys
              , hc_local_delayed     = ck_local_delayed keys
              }
        spec <- hex raw >>= decodeTx
        htx <- need "no htlc tx" (build_htlc_tx hctx)
        encode_htlc_tx htx @?= BT.to_bytes_legacy spec
        -- the input witness
        w <- witnessOf spec
        let rev = ck_revocation_pubkey keys
            rh = ck_remote_htlc keys
            lh = ck_local_htlc keys
            ph = htlc_payment_hash htlc
        case (kind, w) of
          (HtlcSuccess, [_, rsig, lsig, _, _]) -> do
            pre <- testPreimage i
            let ws = received_htlc_script rev rh lh ph
                       (htlc_cltv_expiry htlc) fmt
            htlc_success_witness rsig lsig pre ws @?= BT.Witness w
            sighashes rsig lsig
          (HtlcTimeout, [_, rsig, lsig, _, _]) -> do
            let ws = offered_htlc_script rev rh lh ph fmt
            htlc_timeout_witness rsig lsig ws @?= BT.Witness w
            sighashes rsig lsig
          _ -> assertFailure "unexpected htlc witness"

    -- the remote signature uses remote_htlc_sighash, the local one
    -- SIGHASH_ALL
    sighashes rsig lsig = do
      let flag = fromIntegral . Sighash.encode_sighash
      fmap snd (BS.unsnoc rsig) @?= Just (flag (remote_htlc_sighash fmt))
      fmap snd (BS.unsnoc lsig) @?= Just (flag Sighash.SIGHASH_ALL)

    kindName HtlcSuccess = "htlc-success tx"
    kindName HtlcTimeout = "htlc-timeout tx"

-- commitment transactions ----------------------------------------------------

simpleVector :: IO CommitVector
simpleVector = case appendix_c of
  (v : _) -> pure v
  [] -> assertFailure "missing Appendix C vectors"

commitmentTests :: TestTree
commitmentTests = testGroup "Commitment transactions" [
    testCase "obscured_commitment_number (Appendix C)" $ do
      o <- PaymentBasepoint <$> pt c_local_payment_basepoint
      a <- PaymentBasepoint <$> pt c_remote_payment_basepoint
      cn <- need "commitment_number" (commitment_number 42)
      obscured_commitment_number o a cn @?= 0x2bb038521914 `xor` 42

  , testCase "obscured commitment number from the accepter's side" $ do
      v <- simpleVector
      opener_ctx <- vectorContext StaticRemotekey v
      spec <- hex (cv_commit_tx v) >>= decodeTx
      -- the accepter's (remote's) commitment, with the owner-relative
      -- payment basepoints swapped and the owner not the funder
      let accepter_ctx = opener_ctx
            { cc_local_payment_bp  = cc_remote_payment_bp opener_ctx
            , cc_remote_payment_bp = cc_local_payment_bp opener_ctx
            , cc_is_funder         = False
            }
      commit <- need "no commitment tx" (build_commitment_tx accepter_ctx)
      let Locktime lt = ctx_locktime commit
          Sequence sq = ctx_input_sequence commit
      lt @?= BT.tx_locktime spec
      [sq] @?= fmap BT.txin_sequence (NE.toList (BT.tx_inputs spec))
      -- the obscured number is 0x2bb038521914 ^ 42
      let obscured = 0x2bb038521914 `xor` 42 :: Word64
      lt @?= 0x20000000 + fromIntegral (obscured `mod` 0x1000000)
      sq @?= 0x80000000 + fromIntegral (obscured `div` 0x1000000)

  , testCase "fee taken from the remote output when it funds" $ do
      v <- simpleVector
      ctx <- vectorContext StaticRemotekey v
      commit <- need "no commitment tx" (build_commitment_tx ctx
                  { cc_is_funder = False })
      -- the 10860 sat base fee comes from to_remote's 3000000 sat
      let values = [ (co_type o, BOLT1.un_satoshi (co_value o))
                   | o <- NE.toList (ctx_outputs commit) ]
      values @?= [(OutputToRemote, 2989140), (OutputToLocal, 7000000)]

  , testCase "no outputs" $ do
      v <- simpleVector
      ctx <- vectorContext Anchors v
      zero <- msat 0
      isNothing (build_commitment_tx ctx
        { cc_to_local_msat = zero, cc_to_remote_msat = zero })
        @?= True

  , testCase "anchors: only the anchor of a materialized output" $ do
      v <- simpleVector
      ctx <- vectorContext Anchors v
      zero <- msat 0
      commit <- need "no commitment tx" (build_commitment_tx ctx
                  { cc_to_remote_msat = zero })
      fmap co_type (NE.toList (ctx_outputs commit))
        @?= [OutputLocalAnchor, OutputToLocal]
  ]

-- HTLC transactions ----------------------------------------------------------

htlcTxTests :: TestTree
htlcTxTests = testGroup "HTLC transactions" [
    testCase "fee exceeding the amount" $ do
      keys <- vectorKeys
      outpoint <- vectorOutpoint
      h <- testHtlc 2   -- offered, 2000 sat
      let ctx = HTLCContext
            { hc_commitment_txid   = BT.op_txid outpoint
            , hc_output_index      = 0
            , hc_htlc              = h
            , hc_to_self_delay     = ToSelfDelay 144
            , hc_feerate           = FeeratePerKw 5000  -- fee 3315
            , hc_format            = StaticRemotekey
            , hc_revocation_pubkey = ck_revocation_pubkey keys
            , hc_local_delayed     = ck_local_delayed keys
            }
      isNothing (build_htlc_tx ctx) @?= True
      -- zero-fee under anchors
      htx <- need "anchors htlc tx" (build_htlc_tx ctx { hc_format = Anchors })
      BOLT1.un_satoshi (htx_output_value htx) @?= 2000
      htx_input_sequence htx @?= Sequence 1
      htx_locktime htx @?= Locktime 502
  ]

-- closing transactions -------------------------------------------------------

p2wpkh :: Word8 -> Script
p2wpkh b = Script (BS.pack [0x00, 0x14] <> BS.replicate 20 b)

closingValues :: ClosingTx -> [(Word64, Script)]
closingValues c =
  [ (BOLT1.un_satoshi (clo_value o), clo_script o)
  | o <- NE.toList (cltx_outputs c) ]

closingTests :: TestTree
closingTests = testGroup "Closing transactions" [
    testGroup "legacy" [
      testCase "fee from the funder, outputs in BIP69 order" $ do
        ctx <- legacy 6000500 4000999 546 1000 True False
        c <- need "no closing tx" (build_legacy_closing_tx ctx)
        closingValues c @?= [(4000, remote_spk), (5000, local_spk)]
        cltx_locktime c @?= Locktime 0
        cltx_input_sequence c @?= Sequence 0xFFFFFFFF
        cltx_version c @?= 2

    , testCase "fee from the remote output when it funds" $ do
        ctx <- legacy 6000000 4000000 546 1000 False False
        c <- need "no closing tx" (build_legacy_closing_tx ctx)
        closingValues c @?= [(3000, remote_spk), (6000, local_spk)]

    , testCase "the signer's dust limit applies to both outputs" $ do
        ctx <- legacy 100000000 600000 1000 500 True False
        c <- need "no closing tx" (build_legacy_closing_tx ctx)
        closingValues c @?= [(99500, local_spk)]

    , testCase "the signer may omit its own output" $ do
        ctx <- legacy 6000000 4000000 546 1000 True True
        c <- need "no closing tx" (build_legacy_closing_tx ctx)
        closingValues c @?= [(4000, remote_spk)]

    , testCase "fee exceeding the funder's balance" $ do
        ctx <- legacy 999999 4000000 546 1000 True False
        isNothing (build_legacy_closing_tx ctx) @?= True

    , testCase "no outputs" $ do
        ctx <- legacy 500000 500000 546 0 True False
        isNothing (build_legacy_closing_tx ctx) @?= True
    ]

  , testGroup "option_simple_close" [
      testCase "closer pays the fee, outputs in BIP69 order" $ do
        ctx <- simple 10000999 5000000 500 CloserAndCloseeOutputs
        c <- need "no closing tx" (build_closing_tx ctx)
        closingValues c @?= [(5000, remote_spk), (9500, local_spk)]
        cltx_locktime c @?= Locktime 800000
        cltx_input_sequence c @?= Sequence 0xFFFFFFFD
        cltx_version c @?= 2

    , testCase "closer output only" $ do
        ctx <- simple 10000000 5000000 500 CloserOutputOnly
        c <- need "no closing tx" (build_closing_tx ctx)
        closingValues c @?= [(9500, local_spk)]

    , testCase "closee output only" $ do
        ctx <- simple 10000000 5000000 500 CloseeOutputOnly
        c <- need "no closing tx" (build_closing_tx ctx)
        closingValues c @?= [(5000, remote_spk)]

    , testCase "OP_RETURN outputs have amount zero and are kept" $ do
        ctx <- simple 10000000 5000000 500 CloserAndCloseeOutputs
        let op_return = Script (BS.pack [0x6a, 0x01, 0x00])
        c <- need "no closing tx" (build_closing_tx ctx
               { clc_closer_script = op_return })
        closingValues c @?= [(0, op_return), (5000, remote_spk)]
        c' <- need "no closing tx" (build_closing_tx ctx
                { clc_closee_script = op_return })
        closingValues c' @?= [(0, op_return), (9500, local_spk)]

    , testCase "dust outputs are not trimmed" $ do
        ctx <- simple 10000000 100000 500 CloserAndCloseeOutputs
        c <- need "no closing tx" (build_closing_tx ctx)
        closingValues c @?= [(100, remote_spk), (9500, local_spk)]

    , testCase "fee exceeding the closer's balance" $ do
        ctx <- simple 499999 5000000 500 CloseeOutputOnly
        isNothing (build_closing_tx ctx) @?= True
    ]
  ]
  where
    local_spk = p2wpkh 0x11
    remote_spk = p2wpkh 0x22

    legacy l r dust fee funder omit = do
      outpoint <- vectorOutpoint
      lm <- msat l
      rm <- msat r
      d <- DustLimit <$> sat dust
      f <- sat fee
      pure LegacyClosingContext
        { lcc_funding_outpoint = outpoint
        , lcc_local_msat       = lm
        , lcc_remote_msat      = rm
        , lcc_local_script     = local_spk
        , lcc_remote_script    = remote_spk
        , lcc_dust_limit       = d
        , lcc_fee              = f
        , lcc_is_funder        = funder
        , lcc_omit_local       = omit
        }

    simple closer closee fee outs = do
      outpoint <- vectorOutpoint
      cm <- msat closer
      em <- msat closee
      f <- sat fee
      pure ClosingContext
        { clc_funding_outpoint = outpoint
        , clc_closer_msat      = cm
        , clc_closee_msat      = em
        , clc_closer_script    = local_spk
        , clc_closee_script    = remote_spk
        , clc_fee              = f
        , clc_locktime         = Locktime 800000
        , clc_outputs          = outs
        }

-- scripts --------------------------------------------------------------------

scriptTests :: TestTree
scriptTests = testGroup "Scripts and witnesses" [
    testCase "to_remote scriptPubKeys" $ do
      pk <- RemotePubkey <$> pt c_remote_payment_basepoint
      let Script ws = to_remote_witness_script pk
      -- <remotepubkey> OP_CHECKSIGVERIFY 1 OP_CHECKSEQUENCEVERIFY
      BS.length ws @?= 37
      BS.drop 34 ws @?= BS.pack [0xad, 0x51, 0xb2]
      to_remote_script_pubkey pk Anchors @?= to_p2wsh (Script ws)
      -- P2WPKH, as in Appendix C's to_remote output
      v <- simpleVector
      spec <- hex (cv_commit_tx v) >>= decodeTx
      let p2wpkhs = [ spk | BT.TxOut _ spk <- NE.toList (BT.tx_outputs spec)
                          , BS.length spk == 22 ]
      [to_remote_script_pubkey pk StaticRemotekey] @?= fmap Script p2wpkhs

  , testCase "to_remote witnesses" $ do
      pk@(RemotePubkey p) <- RemotePubkey <$> pt c_remote_payment_basepoint
      to_remote_witness "sig" pk StaticRemotekey
        @?= BT.Witness ["sig", unpoint p]
      let Script ws = to_remote_witness_script pk
      to_remote_witness "sig" pk Anchors @?= BT.Witness ["sig", ws]

  , testCase "to_local witnesses" $ do
      keys <- vectorKeys
      let s@(Script ws) = to_local_script (ck_revocation_pubkey keys)
            (ToSelfDelay 144) (ck_local_delayed keys)
      to_local_witness_spend "sig" s @?= BT.Witness ["sig", "", ws]
      to_local_witness_revoke "sig" s @?= BT.Witness ["sig", "\x01", ws]

  , testCase "anchor witnesses" $ do
      keys <- vectorKeys
      let fpk = ck_local_funding keys
          Script ws = anchor_script fpk
      anchor_witness_owner "sig" fpk @?= BT.Witness ["sig", ws]
      anchor_witness_anyone fpk @?= BT.Witness ["", ws]

  , testCase "HTLC output witnesses" $ do
      keys <- vectorKeys
      h <- testHtlc 2
      pre <- testPreimage 2
      let rev@(RevocationPubkey rp) = ck_revocation_pubkey keys
          off@(Script ows) = offered_htlc_script rev (ck_remote_htlc keys)
            (ck_local_htlc keys) (htlc_payment_hash h) Anchors
          rcv@(Script rws) = received_htlc_script rev (ck_remote_htlc keys)
            (ck_local_htlc keys) (htlc_payment_hash h) (htlc_cltv_expiry h)
            Anchors
      offered_htlc_witness_preimage "sig" pre off
        @?= BT.Witness ["sig", BOLT1.un_payment_preimage pre, ows]
      offered_htlc_witness_revoke "sig" rev off
        @?= BT.Witness ["sig", unpoint rp, ows]
      received_htlc_witness_timeout "sig" rcv
        @?= BT.Witness ["sig", "", rws]
      received_htlc_witness_revoke "sig" rev rcv
        @?= BT.Witness ["sig", unpoint rp, rws]

  , testCase "remote_htlc_sighash" $ do
      remote_htlc_sighash StaticRemotekey @?= Sighash.SIGHASH_ALL
      remote_htlc_sighash Anchors @?= Sighash.SIGHASH_SINGLE_ANYONECANPAY

  , testCase "CSV delays and CLTV expiries are minimally encoded" $ do
      keys <- vectorKeys
      h <- testHtlc 0
      let rev = ck_revocation_pubkey keys
          delayed = ck_local_delayed keys
          delay d = let Script s = to_local_script rev (ToSelfDelay d) delayed
                    in  BS.take 4 (BS.drop 36 s)
          expiry e =
            let Script s = received_htlc_script rev (ck_remote_htlc keys)
                  (ck_local_htlc keys) (htlc_payment_hash h) (CltvExpiry e)
                  StaticRemotekey
            in  BS.take 5 (BS.drop 130 s)
      delay 16 @?= BS.pack [0x60, 0xb2, 0x75, 0x21]
      delay 17 @?= BS.pack [0x01, 0x11, 0xb2, 0x75]
      delay 0x80 @?= BS.pack [0x02, 0x80, 0x00, 0xb2]
      delay 0x8000 @?= BS.pack [0x03, 0x00, 0x80, 0x00]
      expiry 500 @?= BS.pack [0x02, 0xf4, 0x01, 0xb1, 0x75]
      expiry 0x800000 @?= BS.pack [0x04, 0x00, 0x00, 0x80, 0x00]
  ]

-- fees and trimming ----------------------------------------------------------

feeTests :: TestTree
feeTests = testGroup "Fees and trimming (BOLT #3 fee example)" [
    testCase "HTLC transaction fees" $ do
      fmap BOLT1.un_satoshi
        [ htlc_timeout_fee rate StaticRemotekey
        , htlc_success_fee rate StaticRemotekey
        , htlc_timeout_fee rate Anchors
        , htlc_success_fee rate Anchors ]
        @?= [3315, 3515, 0, 0]

  , testCase "trim thresholds" $ do
      d <- DustLimit <$> sat 546
      fmap (BOLT1.un_satoshi . htlc_trim_threshold d rate StaticRemotekey)
        [HTLCOffered, HTLCReceived] @?= [3861, 4061]

  , testCase "trimmed HTLCs and base fee" $ do
      d <- DustLimit <$> sat 546
      hs <- sequence
        [ example HTLCOffered 5000000, example HTLCOffered 1000000
        , example HTLCReceived 7000000, example HTLCReceived 800000 ]
      fmap (is_trimmed d rate StaticRemotekey) hs
        @?= [False, True, False, True]
      BOLT1.un_satoshi (commitment_fee rate StaticRemotekey 2) @?= 5340
      BOLT1.un_satoshi (commitment_fee rate Anchors 0) @?= 5620

  , testCase "commitment fee saturates" $
      commitment_fee (FeeratePerKw maxBound) StaticRemotekey maxBound
        @?= BOLT1.max_satoshi
  ]
  where
    rate = FeeratePerKw 5000
    example dir amt = do
      a <- msat amt
      ph <- need "payment hash" (BOLT1.payment_hash (BS.replicate 32 0))
      pure (HTLC dir a ph (CltvExpiry 500))

-- smart constructors ---------------------------------------------------------

constructorTests :: TestTree
constructorTests = testGroup "Smart constructors" [
    testCase "commitment_number" $ do
      isJust (commitment_number 0) @?= True
      isJust (commitment_number 0xFFFFFFFFFFFF) @?= True
      isNothing (commitment_number 0x1000000000000) @?= True
  , testCase "next_commitment_number" $ do
      cn <- need "commitment_number" (commitment_number 0xFFFFFFFFFFFE)
      fmap un_commitment_number (next_commitment_number cn)
        @?= Just 0xFFFFFFFFFFFF
      top <- need "commitment_number" (commitment_number 0xFFFFFFFFFFFF)
      isNothing (next_commitment_number top) @?= True
  , testCase "secret_index" $ do
      isJust (secret_index 0xFFFFFFFFFFFF) @?= True
      isNothing (secret_index 0x1000000000000) @?= True
  , testCase "seed" $ do
      isJust (seed (BS.replicate 32 0)) @?= True
      isNothing (seed (BS.replicate 31 0)) @?= True
      isNothing (seed (BS.replicate 33 0)) @?= True
  , testCase "seckey" $ do
      isJust (seckey (BS.replicate 32 0x01)) @?= True
      isNothing (seckey (BS.replicate 31 0x01)) @?= True
      isNothing (seckey (BS.replicate 32 0x00)) @?= True
      -- the group order n is out of range, n - 1 in range
      n <- hex
        "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141"
      n1 <- hex
        "fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364140"
      isNothing (seckey n) @?= True
      isJust (seckey n1) @?= True
  ]

-- properties -----------------------------------------------------------------

propertyTests :: TestTree
propertyTests = testGroup "Properties" [
    testProperty "derive_privkey matches derive_pubkey" propPrivkey
  , testProperty "derive_revocationprivkey matches derive_revocationpubkey"
      propRevocationPrivkey
  , testProperty "derive_per_commitment_point' = derive_per_commitment_point"
      propPcpWnaf
  , testProperty "derive_commitment_keys' = derive_commitment_keys"
      propCommitmentKeysWnaf
  , testProperty "every inserted secret is derivable, before and after \
                 \a round trip through un_secret_store" propSecretStore
  , testProperty "commitment_number accepts exactly 48-bit values"
      propCommitmentNumber
  , testProperty "commitment_secret_index counts down from 2^48 - 1"
      propSecretIndex
  ]

genBytes :: Int -> Gen BS.ByteString
genBytes n = BS.pack <$> vectorOf n arbitrary

-- | A random valid secret key.
genSeckey :: Gen Seckey
genSeckey = do
  bs <- genBytes 32
  maybe genSeckey pure (seckey bs)

-- | A random point (as the public key of a random secret key).
genPoint :: Gen BOLT1.Point
genPoint = do
  k <- genSeckey
  case pubkey_of k >>= BOLT1.point of
    Just p  -> pure p
    Nothing -> genPoint

genSecret :: Gen BOLT1.PerCommitmentSecret
genSecret = do
  k <- genSeckey
  maybe genSecret pure (BOLT1.per_commitment_secret (un_seckey k))

genBasepoints :: Gen Basepoints
genBasepoints = Basepoints
  <$> fmap RevocationBasepoint genPoint
  <*> fmap PaymentBasepoint genPoint
  <*> fmap DelayedPaymentBasepoint genPoint
  <*> fmap HtlcBasepoint genPoint

propPrivkey :: Property
propPrivkey = forAll genSeckey $ \k -> forAll genPoint $ \p ->
  let pcp = PerCommitmentPoint p
      bp = pubkey_of k >>= BOLT1.point
  in  (pubkey_of =<< derive_privkey k pcp)
        === fmap unpoint (bp >>= \b -> derive_pubkey b pcp)

propRevocationPrivkey :: Property
propRevocationPrivkey = forAll genSeckey $ \k -> forAll genSecret $ \s ->
  let rbp = fmap RevocationBasepoint (pubkey_of k >>= BOLT1.point)
      pcp = derive_per_commitment_point s
      expected = do
        r <- rbp
        c <- pcp
        RevocationPubkey p <- derive_revocationpubkey r c
        pure (unpoint p)
  in  (pubkey_of =<< derive_revocationprivkey k s) === expected

propPcpWnaf :: Property
propPcpWnaf = forAll genSecret $ \s ->
  derive_per_commitment_point' tex s === derive_per_commitment_point s

propCommitmentKeysWnaf :: Property
propCommitmentKeysWnaf =
  forAll genBasepoints $ \l -> forAll genBasepoints $ \r ->
  forAll genPoint $ \lf -> forAll genPoint $ \rf -> forAll genPoint $ \p ->
    let args f = f l (FundingPubkey lf) r (FundingPubkey rf)
                   (PerCommitmentPoint p)
    in  args (derive_commitment_keys' tex) === args derive_commitment_keys

-- | After inserting a prefix of a correct sequence (descending from
--   2^48-1, all from one seed), every inserted index is derivable, and
--   stays so after a round trip through the store's entries.
propSecretStore :: Property
propSecretStore =
  forAll (genBytes 32) $ \sd ->
  forAll (choose (1, 600)) $ \n ->
    case seed sd of
      Nothing -> counterexample "seed" False
      Just s ->
        let idxs = [ i | Just i <- fmap secret_index
                       (take n [0xFFFFFFFFFFFF, 0xFFFFFFFFFFFE ..]) ]
            secret = generate_from_seed s
            insert st i = st >>= insert_secret (secret i) i
            derivable st = conjoin
              [ fmap unsecret (derive_old_secret i st)
                  === Just (unsecret (secret i))
              | i <- idxs ]
        in  case foldl insert (Just empty_secret_store) idxs of
              Nothing -> counterexample "insert_secret rejected" False
              Just store -> case secret_store (un_secret_store store) of
                Nothing -> counterexample "secret_store rejected" False
                Just store' -> derivable store .&&. derivable store'

propCommitmentNumber :: Property
propCommitmentNumber = forAll (choose (0, maxBound)) $ \n ->
  case commitment_number n of
    Just cn -> n <= 0xFFFFFFFFFFFF && un_commitment_number cn == n
    Nothing -> n > 0xFFFFFFFFFFFF

propSecretIndex :: Property
propSecretIndex = forAll (choose (0, 0xFFFFFFFFFFFF)) $ \n ->
  case commitment_number n of
    Nothing -> counterexample "commitment_number" False
    Just cn ->
      un_secret_index (commitment_secret_index cn) === 0xFFFFFFFFFFFF - n
