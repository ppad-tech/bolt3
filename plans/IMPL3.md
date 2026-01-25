# IMPL3: Benchmark Suite Expansion

## Overview

Implementation plan for expanding benchmark coverage per ARCH3.

## Step 0: NFData Instances

Add NFData instances for all types that will be benchmarked. These are
required for both criterion (`nf`) and weigh (`func`).

Add to `bench/Main.hs` and `bench/Weight.hs`:

```haskell
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

instance NFData Witness where
  rnf (Witness items) = rnf items

instance NFData Outpoint where
  rnf (Outpoint t i) = rnf t `seq` rnf i

instance NFData Sequence where
  rnf (Sequence x) = rnf x

instance NFData Locktime where
  rnf (Locktime x) = rnf x

instance NFData TxId where
  rnf (TxId bs) = rnf bs

instance NFData ToSelfDelay where
  rnf (ToSelfDelay x) = rnf x

instance NFData CommitmentNumber where
  rnf (CommitmentNumber x) = rnf x

-- Parsing types
instance NFData RawTx where
  rnf (RawTx v i o w l) =
    rnf v `seq` rnf i `seq` rnf o `seq` rnf w `seq` rnf l

instance NFData RawInput where
  rnf (RawInput o scr seq) = rnf o `seq` rnf scr `seq` rnf seq

instance NFData RawOutput where
  rnf (RawOutput v s) = rnf v `seq` rnf s

-- Context types (for env setup verification)
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

instance NFData PerCommitmentSecret where
  rnf (PerCommitmentSecret bs) = rnf bs

-- Secret storage
instance NFData SecretStore where
  rnf store = rnf (length store)  -- approximate; avoids deep traversal

instance NFData SecretEntry where
  rnf (SecretEntry i idx sec) = rnf i `seq` rnf idx `seq` rnf sec
```

**Note**: Some of these may already exist or need adjustment based on
actual constructor visibility. Check exports and adjust.

**Independent**: Yes, can be done first before other steps.

## Step 1: Test Fixtures

Define shared test fixtures in `bench/Main.hs`. Use `env` for expensive
setup to exclude from timing.

```haskell
-- Sample pubkeys (33-byte compressed, from BOLT #3 test vectors)
samplePubkey1, samplePubkey2, samplePubkey3 :: Pubkey
samplePubkey1 = Pubkey $ BS.pack [0x03, 0x6d, 0x6c, ...]
-- ... (use actual BOLT #3 test vector keys)

-- Funding outpoint
sampleFundingOutpoint :: Outpoint
sampleFundingOutpoint = Outpoint
  (TxId $ BS.replicate 32 0x01)
  0

-- Sample HTLCs
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
                  (5000000 + i * 100000)
                  (500000 + i)
          | i <- [0..9]]
htlcs100 = [mkHtlc (if even i then HTLCOffered else HTLCReceived)
                   (5000000 + i * 10000)
                   (500000 + i)
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
  , cc_local_payment_bp = PaymentBasepoint samplePubkey1
  , cc_remote_payment_bp = PaymentBasepoint samplePubkey2
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
```

**Independent**: Yes, after Step 0.

## Step 2: Transaction Building Benchmarks

Add to `bench/Main.hs`:

```haskell
bgroup "tx building" [
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
```

Define `sampleHtlcContext` and `sampleClosingContext` fixtures.

**Independent**: Yes, after Step 1.

## Step 3: Script Generation Benchmarks

Add to `bench/Main.hs`:

```haskell
bgroup "script generation" [
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
```

**Independent**: Yes, after Step 1.

## Step 4: Serialization Benchmarks

Add to `bench/Main.hs`:

```haskell
bgroup "serialization" [
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
```

**Independent**: Yes, after Step 2 (needs tx fixtures).

## Step 5: Parsing Benchmarks

Add to `bench/Main.hs`:

```haskell
bgroup "parsing" [
    env (pure $ encode_tx $ build_commitment_tx $
           mkCommitmentContext htlcs0 noAnchors)
      $ \bs -> bench "decode_tx (0 htlcs)" $ whnf decode_tx bs
  , env (pure $ encode_tx $ build_commitment_tx $
           mkCommitmentContext htlcs10 noAnchors)
      $ \bs -> bench "decode_tx (10 htlcs)" $ whnf decode_tx bs
  , env (pure $ encode_tx $ build_commitment_tx $
           mkCommitmentContext htlcs100 noAnchors)
      $ \bs -> bench "decode_tx (100 htlcs)" $ whnf decode_tx bs
  ]
```

**Independent**: Yes, after Step 4 (needs encoded bytes).

## Step 6: Validation Benchmarks

Add to `bench/Main.hs`:

```haskell
bgroup "validation" [
    env (pure $ build_commitment_tx $ mkCommitmentContext htlcs10 noAnchors)
      $ \tx -> bench "validate_commitment_tx (valid)" $
          whnf (validate_commitment_tx dust noAnchors) tx
  , env (pure $ build_htlc_timeout_tx sampleHtlcContext)
      $ \tx -> bench "validate_htlc_tx" $
          whnf (validate_htlc_tx dust noAnchors) tx
  , env (pure $ build_closing_tx sampleClosingContext)
      $ \tx -> bench "validate_closing_tx" $
          whnf validate_closing_tx tx
  , env (pure $ ctx_outputs $ build_commitment_tx $
           mkCommitmentContext htlcs10 noAnchors)
      $ \outs -> bench "validate_output_ordering" $
          whnf validate_output_ordering outs
  ]
```

**Independent**: Yes, after Step 2.

## Step 7: Secret Storage Benchmarks

Add to `bench/Main.hs`:

```haskell
bgroup "secret storage" [
    bench "insert_secret (first)" $
      whnf (insert_secret empty_store 281474976710655)
           (PerCommitmentSecret $ BS.replicate 32 0xFF)
  , env setupFilledStore $ \store ->
      bench "derive_old_secret (recent)" $
        whnf (derive_old_secret store) 281474976710654
  , env setupFilledStore $ \store ->
      bench "derive_old_secret (old)" $
        whnf (derive_old_secret store) 281474976710600
  ]
  where
    setupFilledStore = pure $ foldl insertOne empty_store [0..99]
    insertOne store i =
      let idx = 281474976710655 - i
          sec = PerCommitmentSecret $ BS.replicate 32 (fromIntegral i)
      in case insert_secret store idx sec of
           Just s -> s
           Nothing -> store
```

**Independent**: Yes, after Step 0.

## Step 8: Output Sorting Benchmarks

Add to `bench/Main.hs`:

```haskell
bgroup "output sorting" [
    env (pure $ ctx_outputs $ build_commitment_tx $
           mkCommitmentContext htlcs10 noAnchors)
      $ \outs -> bench "sort_outputs (10)" $ nf sort_outputs outs
  , env (pure $ ctx_outputs $ build_commitment_tx $
           mkCommitmentContext htlcs100 noAnchors)
      $ \outs -> bench "sort_outputs (100)" $ nf sort_outputs outs
  ]
```

**Independent**: Yes, after Step 2.

## Step 9: Mirror in Weight.hs

Replicate all new benchmarks in `bench/Weight.hs` using `weigh`:

```haskell
-- Transaction building
func "build_commitment_tx (0 htlcs)"
     build_commitment_tx (mkCommitmentContext htlcs0 noAnchors)
func "build_commitment_tx (10 htlcs)"
     build_commitment_tx (mkCommitmentContext htlcs10 noAnchors)
-- ... etc
```

Use same fixtures. Structure allocation tracking to match criterion
groups.

**Independent**: Can proceed in parallel with Steps 2-8.

## Step 10: Verify and Clean Up

- Build and run benchmarks: `cabal bench`
- Verify all benchmarks execute without error
- Check for reasonable timing/allocation values
- Remove any duplicate NFData instances if they conflict with library
- Ensure fixture setup is excluded from measurement via `env`

**Depends on**: All previous steps.

## Parallelization Summary

Independent work items:

1. **Step 0** (NFData instances) — must be first
2. **Step 1** (fixtures) — after Step 0
3. **Steps 2, 3, 7** — after Step 1, independent of each other
4. **Step 4** — after Step 2
5. **Steps 5, 6, 8** — after Step 4, independent of each other
6. **Step 9** — can run in parallel with Steps 2-8
7. **Step 10** — final validation

Recommended parallel execution:

```
Step 0 → Step 1 → ┬→ Step 2 → Step 4 → ┬→ Step 5
                  │                    ├→ Step 6
                  │                    └→ Step 8
                  ├→ Step 3
                  ├→ Step 7
                  └→ Step 9 (parallel throughout)
         ↓
       Step 10
```

## Deliverables

- Expanded `bench/Main.hs` with all benchmark groups
- Expanded `bench/Weight.hs` with matching allocation tracking
- NFData instances for all benchmarked types
- Shared fixture definitions
- All benchmarks passing `cabal bench`
