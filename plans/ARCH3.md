# ARCH3: Benchmark Suite Expansion

## Overview

Expand the benchmark suite to cover the full library surface. Current
benchmarks focus on key derivation, secret generation, fee calculation,
and trimming predicates. Missing coverage for transaction building,
script generation, serialization, parsing, validation, and secret
storage operations.

## Current Coverage

Measured in `bench/Main.hs` and `bench/Weight.hs`:

- Key derivation: `derive_pubkey`, `derive_revocationpubkey`
- Secret generation: `generate_from_seed`
- Fee calculation: `commitment_fee`, `htlc_timeout_fee`, `htlc_success_fee`
- Trimming: `is_trimmed`, `htlc_trim_threshold`

## Proposed Additions

### 1. Transaction Building

Primary entry points for transaction construction. These compose
scripts, compute fees, filter HTLCs, and sort outputs.

Functions:
- `build_commitment_tx` — most complex; involves HTLC filtering, script
  generation, fee deduction, output sorting
- `build_htlc_timeout_tx` — single-input, single-output second-stage tx
- `build_htlc_success_tx` — single-input, single-output second-stage tx
- `build_closing_tx` — cooperative close with BIP69 ordering
- `build_legacy_closing_tx` — legacy cooperative close

Benchmark variants:
- Commitment tx with 0 HTLCs (baseline)
- Commitment tx with 10 HTLCs (realistic)
- Commitment tx with 100 HTLCs (stress)
- With/without option_anchors

### 2. Script Generation

Witness scripts are generated during transaction building but worth
measuring in isolation to identify bottlenecks.

Functions:
- `funding_script` — 2-of-2 multisig
- `to_local_script` — revocation + delayed spend
- `to_remote_script` — P2WPKH or P2WSH depending on anchors
- `anchor_script` — anchor output with CHECKSIG + CSV
- `offered_htlc_script` — offered HTLC with timeout path
- `received_htlc_script` — received HTLC with preimage path

### 3. Serialization

Encoding and decoding are performance-critical for signing workflows
and transaction relay.

Functions:
- `encode_tx` — commitment tx to bytes
- `encode_htlc_tx` — HTLC tx to bytes
- `encode_closing_tx` — closing tx to bytes
- `encode_tx_for_signing` — sighash preimage serialization
- `decode_tx` — parse raw bytes to RawTx

Benchmark variants:
- Encode commitment tx (0, 10, 100 HTLCs)
- Decode commitment tx (0, 10, 100 HTLCs)
- Roundtrip: encode then decode

### 4. Validation

Stateless validation for transaction correctness. Useful to benchmark
as these may be called frequently during channel state updates.

Functions:
- `validate_commitment_tx`
- `validate_htlc_tx`
- `validate_closing_tx`
- `validate_output_ordering`
- `validate_dust_limits`
- `validate_commitment_fee`

### 5. Secret Storage

Per-commitment secret storage uses an efficient tree structure.
Worth measuring insert and derive operations.

Functions:
- `insert_secret` — insert new secret at index
- `derive_old_secret` — derive secret at arbitrary past index

Benchmark variants:
- Insert sequence of 1000 secrets
- Derive secrets at various depths
- Store utilization at near-capacity

### 6. Output Sorting

BIP69 output ordering with CLTV tiebreaker for HTLCs.

Functions:
- `sort_outputs` — sort list of TxOutput

Benchmark variants:
- Sort 10, 100 outputs

## NFData Instances

New types requiring NFData for benchmarking:

- `CommitmentTx`
- `CommitmentContext`
- `CommitmentKeys`
- `HTLCTx`
- `HTLCContext`
- `ClosingTx`
- `ClosingContext`
- `TxOutput`
- `OutputType`
- `Script`
- `Witness`
- `Outpoint`
- `Sequence`
- `Locktime`
- `RawTx`
- `RawInput`
- `RawOutput`
- `SecretStore`
- `SecretEntry`
- Basepoint newtypes (already partially covered)

## Test Fixtures

Realistic fixtures should be defined in a shared `where` block or
helper module. Key fixture components:

1. **Keys**: Valid secp256k1 pubkeys for all roles (local, remote,
   revocation, HTLC, funding). Use test vectors from BOLT #3 appendix.

2. **Funding outpoint**: Fixed txid + vout.

3. **HTLCs**: Lists of 0, 10, 100 HTLCs with varied amounts and expiries.
   Mix of offered/received directions.

4. **Channel parameters**: Realistic values for dust limit (546 sat),
   feerate (5000 sat/kw), to_self_delay (144 blocks).

5. **Commitment context**: Full CommitmentContext with all keys
   populated.

6. **Raw transaction bytes**: Pre-serialized transactions for decode
   benchmarks.

## Benchmark Organization

Organize benchmarks into logical groups matching library modules:

```
main = defaultMain [
    bgroup "key derivation" [ ... ]     -- existing
  , bgroup "secret generation" [ ... ]  -- existing
  , bgroup "secret storage" [ ... ]     -- NEW
  , bgroup "fee calculation" [ ... ]    -- existing
  , bgroup "trimming" [ ... ]           -- existing
  , bgroup "script generation" [ ... ]  -- NEW
  , bgroup "tx building" [ ... ]        -- NEW
  , bgroup "serialization" [ ... ]      -- NEW
  , bgroup "parsing" [ ... ]            -- NEW
  , bgroup "validation" [ ... ]         -- NEW
  , bgroup "output sorting" [ ... ]     -- NEW
  ]
```

## Allocation Tracking

Mirror all criterion benchmarks in `bench/Weight.hs` using weigh.
This helps identify allocation regressions.

## Success Criteria

- All exported transaction building functions benchmarked
- All exported script generation functions benchmarked
- Encode/decode roundtrip for all tx types
- Validation functions with valid and invalid inputs
- Secret storage under realistic load
- NFData instances for all benchmarked types
- No new external dependencies

## Risks

- Large fixture setup may dominate small function benchmarks; use
  `env` to separate setup from measurement
- NFData instances for recursive structures (SecretStore) require care
- Some functions may be too fast to measure reliably; use `whnf` vs
  `nf` appropriately
