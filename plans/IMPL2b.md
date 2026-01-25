# IMPL2b: HTLC Transaction Builder Abstraction

Implements the HTLC transaction builder refactoring from ARCH2.md.

## Scope

- Factor common code from `build_htlc_timeout_tx` and `build_htlc_success_tx`
- No public API changes

## Tasks

### 1. Add internal helper function to Tx.hs

Add a common builder function in `lib/Lightning/Protocol/BOLT3/Tx.hs`
(not exported):

```haskell
-- | Internal helper for HTLC transaction construction.
--
-- Both HTLC-timeout and HTLC-success transactions share the same
-- structure, differing only in locktime and fee calculation.
build_htlc_tx_common
  :: HTLCContext
  -> Locktime           -- ^ Transaction locktime
  -> Satoshi            -- ^ Fee to subtract from output
  -> HTLCTx
build_htlc_tx_common ctx locktime fee =
  let !amountSat = msat_to_sat (htlc_amount_msat $ hc_htlc ctx)
      !outputValue = if unSatoshi amountSat >= unSatoshi fee
                     then Satoshi (unSatoshi amountSat - unSatoshi fee)
                     else Satoshi 0
      !inputSeq = if has_anchors (hc_features ctx)
                   then Sequence 1
                   else Sequence 0
      !outpoint = Outpoint (hc_commitment_txid ctx) (hc_output_index ctx)
      !outputScript = to_p2wsh $ htlc_output_script
        (hc_revocation_pubkey ctx)
        (hc_to_self_delay ctx)
        (hc_local_delayed ctx)
  in HTLCTx
       { htx_version = 2
       , htx_locktime = locktime
       , htx_input_outpoint = outpoint
       , htx_input_sequence = inputSeq
       , htx_output_value = outputValue
       , htx_output_script = outputScript
       }
{-# INLINE build_htlc_tx_common #-}
```

### 2. Refactor build_htlc_timeout_tx

Replace the current implementation with:

```haskell
-- | Build an HTLC-timeout transaction.
--
-- * locktime: cltv_expiry
-- * sequence: 0 (or 1 with option_anchors)
-- * output: to_local style script with revocation and delayed paths
build_htlc_timeout_tx :: HTLCContext -> HTLCTx
build_htlc_timeout_tx ctx =
  let !fee = htlc_timeout_fee (hc_feerate ctx) (hc_features ctx)
      !locktime = Locktime (unCltvExpiry $ htlc_cltv_expiry $ hc_htlc ctx)
  in build_htlc_tx_common ctx locktime fee
{-# INLINE build_htlc_timeout_tx #-}
```

### 3. Refactor build_htlc_success_tx

Replace the current implementation with:

```haskell
-- | Build an HTLC-success transaction.
--
-- * locktime: 0
-- * sequence: 0 (or 1 with option_anchors)
-- * output: to_local style script with revocation and delayed paths
build_htlc_success_tx :: HTLCContext -> HTLCTx
build_htlc_success_tx ctx =
  let !fee = htlc_success_fee (hc_feerate ctx) (hc_features ctx)
  in build_htlc_tx_common ctx (Locktime 0) fee
{-# INLINE build_htlc_success_tx #-}
```

### 4. Verify build and tests pass

```bash
nix develop -c cabal build all
nix develop -c cabal test
```

Existing tests should continue to pass since the public API is unchanged.

### 5. Run benchmarks to verify no regression

```bash
nix develop -c cabal bench bolt3-bench
```

## Files Modified

- `lib/Lightning/Protocol/BOLT3/Tx.hs`

## Commit Message

```
Refactor HTLC transaction builders to share common code

Factor out build_htlc_tx_common helper that handles the shared
transaction structure. build_htlc_timeout_tx and build_htlc_success_tx
now differ only in their locktime and fee parameters, making the
distinction between them explicit.

No public API changes.
```
