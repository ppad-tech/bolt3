# ARCH1: ppad-bolt3 architecture plan

## Goals

- Implement BOLT #3 transaction formats and validation rules in Haskell.
- Provide total, type-safe constructors and parsers for BOLT #3 data.
- Keep dependencies minimal (GHC core/boot + ppad-* libraries).
- Maintain performance with strict fields and unboxed primitives where
  useful.

## Scope

- Channel funding, commitment, HTLC, and closing transaction formats.
- Script templates and witness construction per BOLT #3.
- Parsing and serialization of all BOLT #3 transaction types.
- Fee/weight accounting and dust checks.
- Test vectors from the BOLT spec and related references.

## Non-goals (initial)

- Network-level negotiation (BOLT #2).
- Gossip / routing (BOLT #7, #10).
- On-chain wallet management.
- Full Bitcoin scripting engine.

## Module layout

- `Lightning.Protocol.BOLT3`
  - Re-export public API.

- `Lightning.Protocol.BOLT3.Types`
  - Newtypes for satoshis, milli-satoshis, txid, outpoint, scripts.
  - Invariant-encoding ADTs for transaction variants.

- `Lightning.Protocol.BOLT3.Keys`
  - Key derivation helpers (per-commitment point/secret handling).

- `Lightning.Protocol.BOLT3.Scripts`
  - Script templates (P2WSH, HTLC scripts, to_remote/to_local).
  - Witness construction.

- `Lightning.Protocol.BOLT3.Tx`
  - Transaction assembly for funding/commitment/HTLC/closing.
  - Weight and fee computation.

- `Lightning.Protocol.BOLT3.Encode`
  - Serialization to Bitcoin tx format.

- `Lightning.Protocol.BOLT3.Decode`
  - Parsing from Bitcoin tx format with validation.

- `Lightning.Protocol.BOLT3.Validate`
  - Stateless validation rules (dust limits, script forms, fee rules).

## Data modeling principles

- Use newtypes for all numeric and hash-like primitives.
- Encode illegal states as unrepresentable (ADTs for tx variants).
- Provide smart constructors for all externally supplied data.
- Avoid partial functions; return `Maybe`/`Either` for fallible ops.

## Performance strategy

- Use strict fields with `!` and `UNPACK` where helpful.
- Inline small helpers; use `MagicHash` for hot-path primitives only
  when profiling shows the need.
- Avoid intermediate allocations in serialization.

## External dependencies

- Prefer GHC core/boot libs (base, bytestring, etc.).
- Use ppad-* libraries for primitives (hashing, fixed-size types).
- Ask before adding any external dependency outside policy.

## Testing strategy

- `test/Main.hs` uses tasty.
- Unit tests for each script/tx type using BOLT #3 vectors.
- Property tests for invariants (serialization roundtrip, fee bounds).

## Benchmarking strategy

- `bench/Main.hs`: criterion for serialization and tx assembly.
- `bench/Weight.hs`: weigh for allocations in hot paths.
- Add NFData instances for benchmarked types.

## Open questions

- Which ppad-* libraries should be preferred for hash/bytes types?
- Should we expose low-level Bitcoin tx types or keep them internal?
- Expected API surface: minimal constructors or full builder API?
