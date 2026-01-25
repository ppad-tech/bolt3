# IMPL1: ppad-bolt3 implementation plan

## Milestone 0: groundwork

- Verify project layout and create module stubs under `lib/`.
- Define core newtypes and ADTs in `Lightning.Protocol.BOLT3.Types`.
- Add smart constructors and total helpers.

## Milestone 1: scripts and keys

- Implement per-commitment key helpers in
  `Lightning.Protocol.BOLT3.Keys`.
- Implement script templates in `Lightning.Protocol.BOLT3.Scripts`:
  - to_local, to_remote, HTLC offered/received, anchor outputs.
  - Witness construction helpers.

## Milestone 2: transaction assembly

- Implement tx assembly in `Lightning.Protocol.BOLT3.Tx`:
  - Funding transaction (if needed for completeness).
  - Commitment tx (local/remote).
  - HTLC-timeout and HTLC-success.
  - Closing tx.
- Implement weight and fee accounting helpers.

## Milestone 3: encode/decode

- Implement serialization in `Lightning.Protocol.BOLT3.Encode`.
- Implement parsing + validation in `Lightning.Protocol.BOLT3.Decode`.
- Implement `Lightning.Protocol.BOLT3.Validate` for stateless rules.

## Milestone 4: public API

- Wire re-exports in `Lightning.Protocol.BOLT3`.
- Document exported functions with Haddock examples.

## Milestone 5: tests and benchmarks

- Add BOLT #3 vector tests in `test/Main.hs` (tasty-hunit).
- Add property tests (roundtrip, fee bounds) using tasty-quickcheck.
- Add benchmarks for serialization + assembly in `bench/Main.hs`.
- Add allocation tracking in `bench/Weight.hs`.

## Independent work items

- Keys module can proceed in parallel with Scripts.
- Encode/Decode modules can be developed in parallel after Types.
- Benchmarks can start after Encode/Tx signatures are stable.

## Risks and mitigations

- Spec ambiguities: annotate with links to BOLT #3 sections.
- Performance regressions: keep strictness + benchmark early.
- Validation gaps: cross-check with spec vectors.

## Deliverables

- Full BOLT #3 tx modeling and serialization.
- Validation helpers with total APIs.
- Test suite + benchmarks per project conventions.

## Local references

- If other BOLT implementations are needed, prefer local copies in
  sibling repos (e.g. `../bolt1`) over fetching externally.
