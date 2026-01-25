# ARCH2: Refactoring for Abstraction and Type Safety

## Overview

This document describes refactoring opportunities identified in the ppad-bolt3
library to reduce code duplication and improve type-level invariant
enforcement.

## Goals

1. **Reduce duplication** in HTLC transaction builders and key derivation
2. **Enforce size invariants** on ByteString-based newtypes at construction
3. **Enforce range invariants** on numeric types (48-bit CommitmentNumber)

## Non-Goals

- Changing the public API signatures (beyond adding smart constructors)
- Adding runtime overhead to hot paths
- Breaking existing tests

---

## 1. Size-Constrained ByteString Types

### Current State

Several newtypes wrap `ByteString` without enforcing expected sizes:

| Type                | Expected Size |
|---------------------|---------------|
| `Pubkey`            | 33 bytes      |
| `Point`             | 33 bytes      |
| `Seckey`            | 32 bytes      |
| `TxId`              | 32 bytes      |
| `PaymentHash`       | 32 bytes      |
| `PaymentPreimage`   | 32 bytes      |
| `PerCommitmentSecret` | 32 bytes    |

### Proposed Change

Add smart constructors that validate size, returning `Maybe`:

```haskell
-- | Parse a 33-byte compressed public key.
pubkey :: BS.ByteString -> Maybe Pubkey
pubkey bs
  | BS.length bs == 33 = Just (Pubkey bs)
  | otherwise = Nothing

-- | Parse a 32-byte transaction ID.
txid :: BS.ByteString -> Maybe TxId
txid bs
  | BS.length bs == 32 = Just (TxId bs)
  | otherwise = Nothing
```

The raw constructors remain exported for internal use and for cases where
the caller has already validated the input (e.g., from cryptographic
operations that guarantee output size).

### Impact

- Callers parsing external data get early validation
- No runtime overhead for internal construction paths
- Tests may need updates for smart constructor usage

---

## 2. CommitmentNumber Range Constraint

### Current State

`CommitmentNumber` wraps `Word64` but semantically represents a 48-bit value
(max 2^48 - 1 = 281474976710655).

### Proposed Change

Add a smart constructor that validates the range:

```haskell
-- | Create a commitment number (must be < 2^48).
commitment_number :: Word64 -> Maybe CommitmentNumber
commitment_number n
  | n <= 0xFFFFFFFFFFFF = Just (CommitmentNumber n)
  | otherwise = Nothing
```

### Impact

- Prevents invalid commitment numbers from propagating
- Minimal - most internal uses are already within range

---

## 3. HTLC Transaction Builder Abstraction

### Current State

`build_htlc_timeout_tx` and `build_htlc_success_tx` in `Tx.hs` share ~80%
of their implementation:

- Same output script construction
- Same input/output structure
- Differ only in: locktime source, fee function

### Proposed Change

Factor out a common helper:

```haskell
-- Internal helper for HTLC transaction construction
build_htlc_tx_common
  :: HTLCContext
  -> Locktime           -- ^ Locktime for the transaction
  -> Satoshi            -- ^ Fee to subtract
  -> HTLCTx

build_htlc_timeout_tx :: HTLCContext -> HTLCTx
build_htlc_timeout_tx ctx =
  let !fee = htlc_timeout_fee (hc_feerate ctx) (hc_features ctx)
      !locktime = Locktime (unCltvExpiry $ htlc_cltv_expiry $ hc_htlc ctx)
  in build_htlc_tx_common ctx locktime fee

build_htlc_success_tx :: HTLCContext -> HTLCTx
build_htlc_success_tx ctx =
  let !fee = htlc_success_fee (hc_feerate ctx) (hc_features ctx)
  in build_htlc_tx_common ctx (Locktime 0) fee
```

### Impact

- Reduces code duplication
- Makes the difference between timeout/success explicit
- No API change

---

## 4. Key Derivation Abstraction

### Current State

In `Keys.hs`, several derivation functions are structurally identical:

- `derive_local_htlcpubkey` / `derive_remote_htlcpubkey`
- `derive_local_delayedpubkey` / `derive_remote_delayedpubkey`

Each just unwraps a basepoint, calls `derive_pubkey`, and wraps in a
different newtype.

### Proposed Change

Keep the explicit functions (they provide good type safety and
documentation) but have them delegate to a single internal helper pattern.
The current implementation already does this via `derive_pubkey`, so this
is more about recognizing the pattern is intentional rather than changing
it.

Alternatively, could add a type class but that adds complexity for minimal
gain.

### Decision

**Keep as-is.** The "duplication" is intentional type safety - each function
has a distinct type signature that prevents mixing up local/remote keys.
The actual derivation logic (`derive_pubkey`) is already shared.

---

## Summary of Changes

| Area | Change | Priority |
|------|--------|----------|
| Types.hs | Smart constructors for sized ByteStrings | High |
| Types.hs | Smart constructor for CommitmentNumber | Medium |
| Tx.hs | Factor HTLC tx builder common code | Medium |
| Keys.hs | Keep as-is (already well-factored) | N/A |
