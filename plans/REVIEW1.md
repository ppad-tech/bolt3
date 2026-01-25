# REVIEW1: PTAL findings (master)

## Scope

Review of recent master commits:
- 8af91e3 (Types)
- 583dea5 (Keys)
- 8ed369e (Scripts)
- 5c8e641 (Keys fix)

## Findings

### Critical

1) SecretStore bucket indexing is inconsistent with shachain layout.
   - `SecretStore` stores entries in a plain list without bucket
     positions; `insert_secret` and `derive_old_secret` treat list index
     as bucket index.
   - `insertAt` appends when `length entries <= b`, so inserting bucket 10
     into an empty store yields position 0; `derive_old_secret` then uses
     `b=0` for masking/derivation and produces incorrect results or
     accepts invalid secrets.
   - References:
     - `lib/Lightning/Protocol/BOLT3/Keys.hs:304`
     - `lib/Lightning/Protocol/BOLT3/Keys.hs:331`
     - `lib/Lightning/Protocol/BOLT3/Keys.hs:374`

### High

2) HTLC ordering deviates from BOLT #3.
   - `Ord HTLC` compares amount then `cltv_expiry`, but spec requires
     ordering by amount then `scriptPubKey` lexicographic (with
     `cltv_expiry` impacting script bytes for received HTLCs).
   - This affects output ordering and thus signature preimages.
   - Reference: `lib/Lightning/Protocol/BOLT3/Types.hs:205`

3) `to_remote_witness` missing pubkey in non-anchors case.
   - P2WPKH witness must be `<sig> <pubkey>`; helper returns only
     `<sig>` and relies on caller to append pubkey (not implemented).
   - Reference: `lib/Lightning/Protocol/BOLT3/Scripts.hs:365`

4) `push_cltv` encodes script numbers with reversed endianness.
   - `encode_scriptnum` builds little-endian then reverses twice, yielding
     big-endian output. Values > 16 will be incorrectly encoded.
   - Reference: `lib/Lightning/Protocol/BOLT3/Scripts.hs:191`

## Notes / Open questions

- Witness helpers for P2WSH outputs omit the witness script item; if the
  Tx assembly is expected to append the script, document this explicitly
  to avoid misuse.
- `to_remote_script` returns a witness script for anchors but a
  scriptPubKey for non-anchors; consider splitting APIs or clarifying
  naming.
