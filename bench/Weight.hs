module Main where

import Fixtures
import Lightning.Protocol.BOLT3
import Weigh

main :: IO ()
main = do
  fx <- fixtures
  let (next_idx, next_secret) = fx_store_next fx
  mainWith $ do
    wgroup "keys" $ do
      func "derive_per_commitment_point" derive_per_commitment_point
        (fx_secret fx)
      func "derive_per_commitment_point'" (derive_per_commitment_point' tex)
        (fx_secret fx)
      func "derive_pubkey" (derive_pubkey (fx_basepoint fx)) (fx_pcp fx)
      func "derive_revocationpubkey" (derive_revocationpubkey (fx_rbp fx))
        (fx_pcp fx)
      func "derive_commitment_keys"
        (derive_commitment_keys (fx_local_bps fx) (fx_local_fund fx)
           (fx_remote_bps fx) (fx_remote_fund fx))
        (fx_pcp fx)
      func "derive_commitment_keys'"
        (derive_commitment_keys' tex (fx_local_bps fx) (fx_local_fund fx)
           (fx_remote_bps fx) (fx_remote_fund fx))
        (fx_pcp fx)
      func "derive_privkey" (derive_privkey (fx_seckey fx)) (fx_pcp fx)
      func "derive_revocationprivkey"
        (derive_revocationprivkey (fx_seckey fx)) (fx_secret fx)
    wgroup "secrets" $ do
      func "generate_from_seed (index 2^48 - 1)"
        (generate_from_seed (fx_seed fx)) (fx_store_oldest fx)
      func "insert_secret (14 lower buckets)"
        (insert_secret next_secret next_idx) (fx_store fx)
      func "derive_old_secret (index 2^48 - 1)"
        (derive_old_secret (fx_store_oldest fx)) (fx_store fx)
      func "secret_store . un_secret_store"
        (secret_store . un_secret_store) (fx_store fx)
    wgroup "transactions" $ do
      func "build_commitment_tx (no htlcs)" build_commitment_tx
        (fx_ctx_simple fx)
      func "build_commitment_tx (5 htlcs)" build_commitment_tx
        (fx_ctx_htlcs fx)
      func "build_commitment_tx (5 htlcs, anchors)" build_commitment_tx
        (fx_ctx_anchors fx)
      func "encode_commitment_tx (5 htlcs)" encode_commitment_tx
        (fx_commit fx)
      func "build_htlc_tx" build_htlc_tx (fx_htlc_ctx fx)
      func "encode_htlc_tx" encode_htlc_tx (fx_htlc_tx fx)
      func "build_closing_tx" build_closing_tx (fx_closing fx)
      func "build_legacy_closing_tx" build_legacy_closing_tx (fx_legacy fx)
      func "encode_closing_tx" encode_closing_tx (fx_closing_tx fx)
