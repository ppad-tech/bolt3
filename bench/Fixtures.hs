{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Fixtures (
    Fixtures(..)
  , fixtures
  , tex
  ) where

import qualified Bitcoin.Prim.Tx as BT
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (foldM)
import qualified Crypto.Curve.Secp256k1 as S
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import GHC.Generics (Generic)
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT3

-- | Shared wNAF context.
tex :: S.Context
tex = S.precompute
{-# NOINLINE tex #-}

data Fixtures = Fixtures
  { fx_secret       :: !BOLT1.PerCommitmentSecret
  , fx_seed         :: !Seed
  , fx_basepoint    :: !BOLT1.Point
  , fx_pcp          :: !PerCommitmentPoint
  , fx_rbp          :: !RevocationBasepoint
  , fx_seckey       :: !Seckey
  , fx_local_bps    :: !Basepoints
  , fx_remote_bps   :: !Basepoints
  , fx_local_fund   :: !FundingPubkey
  , fx_remote_fund  :: !FundingPubkey
  , fx_store        :: !SecretStore
  , fx_store_next   :: !(SecretIndex, BOLT1.PerCommitmentSecret)
  , fx_store_oldest :: !SecretIndex
  , fx_ctx_simple   :: !CommitmentContext
  , fx_ctx_htlcs    :: !CommitmentContext
  , fx_ctx_anchors  :: !CommitmentContext
  , fx_commit       :: !CommitmentTx
  , fx_htlc_ctx     :: !HTLCContext
  , fx_htlc_tx      :: !HTLCTx
  , fx_closing      :: !ClosingContext
  , fx_legacy       :: !LegacyClosingContext
  , fx_closing_tx   :: !ClosingTx
  } deriving Generic

instance NFData Fixtures

hex :: BS.ByteString -> Maybe BS.ByteString
hex = B16.decode

pt :: BS.ByteString -> Maybe BOLT1.Point
pt h = hex h >>= BOLT1.point

-- | Benchmark fixtures, from the BOLT #3 Appendix C parameters, fully
--   evaluated.
fixtures :: IO Fixtures
fixtures = maybe (fail "invalid fixtures") (evaluate . force) build

build :: Maybe Fixtures
build = do
  secret <- BOLT1.per_commitment_secret (BS.pack [31, 30 .. 0])
  sd <- seed (BS.replicate 32 0xff)
  basepoint <- pt
    "036d6caac248af96f6afa7f904f550253a0f3ef3f5aa2fe6838a95b216691468e2"
  pcp <- PerCommitmentPoint <$> pt
    "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"
  sk <- seckey (BS.pack [0 .. 31])

  local_pay <- pt
    "034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa"
  remote_pay <- pt
    "032c0b7cf95324a07d05398b240174dc0c2be444d96b159aa6c7f7b1e668680991"
  local_delayed <- pt
    "023c72addb4fdf09af94f0c94d7fe92a386a7e70cf8a1d85916386bb2535c7b1b1"
  remote_rev <- pt
    "02466d7fcae563e5cb09a0d1870bb580344804617879a14949cf22285f1bae3f27"
  local_fund <- FundingPubkey <$> pt
    "023da092f6980e58d2c037173180e9a465476026ee50f96695963e8efe436f54eb"
  remote_fund <- FundingPubkey <$> pt
    "030e9f7b623d2ccc7c9bd44d66d5ce21ce504c0acf6385a132cec6d3c39fa711c1"
  let local_bps = Basepoints (RevocationBasepoint local_pay)
        (PaymentBasepoint local_pay) (DelayedPaymentBasepoint local_delayed)
        (HtlcBasepoint local_pay)
      remote_bps = Basepoints (RevocationBasepoint remote_rev)
        (PaymentBasepoint remote_pay) (DelayedPaymentBasepoint remote_pay)
        (HtlcBasepoint remote_pay)
  keys <- derive_commitment_keys local_bps local_fund remote_bps remote_fund
            pcp

  -- a store holding the 2^14 - 1 most recent secrets; the next index has
  -- 14 trailing zeros, so inserting it checks every lower bucket
  let n = 16383 :: Int
  idxs <- traverse secret_index (take n [0xFFFFFFFFFFFF, 0xFFFFFFFFFFFE ..])
  store <- foldM (\st i -> insert_secret (generate_from_seed sd i) i st)
             empty_secret_store idxs
  next <- secret_index (0xFFFFFFFFFFFF - fromIntegral n)
  oldest <- secret_index 0xFFFFFFFFFFFF

  txid <- hex
    "8984484a580b825b9972d7adb15050b3ab624ccd731946b3eeddb92f4e7ef6be"
  outpoint <- (\t -> BT.OutPoint t 0) <$> BT.mk_txid (BS.reverse txid)
  cn <- commitment_number 42
  dust <- DustLimit <$> BOLT1.satoshi 546
  to_local <- BOLT1.milli_satoshi 6988000000
  to_remote <- BOLT1.milli_satoshi 3000000000
  htlcs <- traverse htlc
    [ (HTLCReceived, 1000000, 500, 0x00), (HTLCReceived, 2000000, 501, 0x01)
    , (HTLCOffered, 2000000, 502, 0x02), (HTLCOffered, 3000000, 503, 0x03)
    , (HTLCReceived, 4000000, 504, 0x04) ]
  let ctx_simple = CommitmentContext
        { cc_funding_outpoint  = outpoint
        , cc_commitment_number = cn
        , cc_local_payment_bp  = PaymentBasepoint local_pay
        , cc_remote_payment_bp = PaymentBasepoint remote_pay
        , cc_to_self_delay     = ToSelfDelay 144
        , cc_dust_limit        = dust
        , cc_feerate           = FeeratePerKw 15000
        , cc_format            = StaticRemotekey
        , cc_is_funder         = True
        , cc_to_local_msat     = to_local
        , cc_to_remote_msat    = to_remote
        , cc_htlcs             = []
        , cc_keys              = keys
        }
      ctx_htlcs = ctx_simple { cc_feerate = FeeratePerKw 644
                             , cc_htlcs = htlcs }
      ctx_anchors = ctx_htlcs { cc_format = Anchors }
  commit <- build_commitment_tx ctx_htlcs
  htlc_out <- case htlcs of
    (h : _) -> Just h
    []      -> Nothing
  let htlc_ctx = HTLCContext
        { hc_commitment_txid   = BT.txid (commitment_to_tx commit)
        , hc_output_index      = 0
        , hc_htlc              = htlc_out
        , hc_to_self_delay     = ToSelfDelay 144
        , hc_feerate           = FeeratePerKw 644
        , hc_format            = StaticRemotekey
        , hc_revocation_pubkey = ck_revocation_pubkey keys
        , hc_local_delayed     = ck_local_delayed keys
        }
  htlc_tx <- build_htlc_tx htlc_ctx

  fee <- BOLT1.satoshi 1000
  let p2wpkh b = Script (BS.pack [0x00, 0x14] <> BS.replicate 20 b)
      closing = ClosingContext
        { clc_funding_outpoint = outpoint
        , clc_closer_msat      = to_local
        , clc_closee_msat      = to_remote
        , clc_closer_script    = p2wpkh 0x11
        , clc_closee_script    = p2wpkh 0x22
        , clc_fee              = fee
        , clc_locktime         = Locktime 0
        , clc_outputs          = CloserAndCloseeOutputs
        }
      legacy = LegacyClosingContext
        { lcc_funding_outpoint = outpoint
        , lcc_local_msat       = to_local
        , lcc_remote_msat      = to_remote
        , lcc_local_script     = p2wpkh 0x11
        , lcc_remote_script    = p2wpkh 0x22
        , lcc_dust_limit       = dust
        , lcc_fee              = fee
        , lcc_is_funder        = True
        , lcc_omit_local       = False
        }
  closing_tx <- build_closing_tx closing

  pure Fixtures
    { fx_secret       = secret
    , fx_seed         = sd
    , fx_basepoint    = basepoint
    , fx_pcp          = pcp
    , fx_rbp          = RevocationBasepoint basepoint
    , fx_seckey       = sk
    , fx_local_bps    = local_bps
    , fx_remote_bps   = remote_bps
    , fx_local_fund   = local_fund
    , fx_remote_fund  = remote_fund
    , fx_store        = store
    , fx_store_next   = (next, generate_from_seed sd next)
    , fx_store_oldest = oldest
    , fx_ctx_simple   = ctx_simple
    , fx_ctx_htlcs    = ctx_htlcs
    , fx_ctx_anchors  = ctx_anchors
    , fx_commit       = commit
    , fx_htlc_ctx     = htlc_ctx
    , fx_htlc_tx      = htlc_tx
    , fx_closing      = closing
    , fx_legacy       = legacy
    , fx_closing_tx   = closing_tx
    }
  where
    htlc (dir, amt, expiry, b) = do
      a <- BOLT1.milli_satoshi amt
      ph <- BOLT1.payment_hash (SHA256.hash (BS.replicate 32 b))
      pure (HTLC dir a ph (CltvExpiry expiry))
