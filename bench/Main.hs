module Main where

import Criterion.Main
import Fixtures
import Lightning.Protocol.BOLT3

main :: IO ()
main = do
  fx <- fixtures
  let (next_idx, next_secret) = fx_store_next fx
      keys = ck fx
  defaultMain [
      bgroup "keys" [
        bench "derive_per_commitment_point" $
          nf derive_per_commitment_point (fx_secret fx)
      , bench "derive_per_commitment_point'" $
          nf (derive_per_commitment_point' tex) (fx_secret fx)
      , bench "derive_pubkey" $
          nf (derive_pubkey (fx_basepoint fx)) (fx_pcp fx)
      , bench "derive_revocationpubkey" $
          nf (derive_revocationpubkey (fx_rbp fx)) (fx_pcp fx)
      , bench "derive_commitment_keys" $
          nf (derive_commitment_keys (fx_local_bps fx) (fx_local_fund fx)
                (fx_remote_bps fx) (fx_remote_fund fx)) (fx_pcp fx)
      , bench "derive_commitment_keys'" $
          nf (derive_commitment_keys' tex (fx_local_bps fx) (fx_local_fund fx)
                (fx_remote_bps fx) (fx_remote_fund fx)) (fx_pcp fx)
      , bench "derive_privkey" $
          nf (derive_privkey (fx_seckey fx)) (fx_pcp fx)
      , bench "derive_revocationprivkey" $
          nf (derive_revocationprivkey (fx_seckey fx)) (fx_secret fx)
      ]
    , bgroup "secrets" [
        bench "generate_from_seed (index 2^48 - 1)" $
          nf (generate_from_seed (fx_seed fx)) (fx_store_oldest fx)
      , bench "insert_secret (14 lower buckets)" $
          nf (insert_secret next_secret next_idx) (fx_store fx)
      , bench "derive_old_secret (index 2^48 - 1)" $
          nf (derive_old_secret (fx_store_oldest fx)) (fx_store fx)
      , bench "secret_store . un_secret_store" $
          nf (secret_store . un_secret_store) (fx_store fx)
      ]
    , bgroup "scripts" [
        bench "funding_script" $
          nf (funding_script (fx_local_fund fx)) (fx_remote_fund fx)
      , bench "to_local_script" $
          nf (to_local_script (ck_revocation_pubkey keys) (ToSelfDelay 144))
             (ck_local_delayed keys)
      , bench "offered_htlc_script" $
          nf (offered_htlc_script (ck_revocation_pubkey keys)
                (ck_remote_htlc keys) (ck_local_htlc keys) (hash fx))
             Anchors
      , bench "received_htlc_script" $
          nf (received_htlc_script (ck_revocation_pubkey keys)
                (ck_remote_htlc keys) (ck_local_htlc keys) (hash fx)
                (CltvExpiry 500))
             Anchors
      ]
    , bgroup "transactions" [
        bench "build_commitment_tx (no htlcs)" $
          nf build_commitment_tx (fx_ctx_simple fx)
      , bench "build_commitment_tx (5 htlcs)" $
          nf build_commitment_tx (fx_ctx_htlcs fx)
      , bench "build_commitment_tx (5 htlcs, anchors)" $
          nf build_commitment_tx (fx_ctx_anchors fx)
      , bench "encode_commitment_tx (5 htlcs)" $
          nf encode_commitment_tx (fx_commit fx)
      , bench "build_htlc_tx" $
          nf build_htlc_tx (fx_htlc_ctx fx)
      , bench "encode_htlc_tx" $
          nf encode_htlc_tx (fx_htlc_tx fx)
      , bench "build_closing_tx" $
          nf build_closing_tx (fx_closing fx)
      , bench "build_legacy_closing_tx" $
          nf build_legacy_closing_tx (fx_legacy fx)
      , bench "encode_closing_tx" $
          nf encode_closing_tx (fx_closing_tx fx)
      ]
    ]
  where
    ck = cc_keys . fx_ctx_simple
    hash = htlc_payment_hash . hc_htlc . fx_htlc_ctx
