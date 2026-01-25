# IMPL2a: Smart Constructors for Type Safety

Implements the type-level invariant enforcement from ARCH2.md.

## Scope

- Add smart constructors for size-constrained ByteString newtypes
- Add smart constructor for CommitmentNumber (48-bit range)
- Update exports in Types.hs and BOLT3.hs

## Tasks

### 1. Add smart constructors to Types.hs

Add the following smart constructors in `lib/Lightning/Protocol/BOLT3/Types.hs`:

```haskell
-- 33-byte types
pubkey :: BS.ByteString -> Maybe Pubkey
point :: BS.ByteString -> Maybe Point

-- 32-byte types
seckey :: BS.ByteString -> Maybe Seckey
txid :: BS.ByteString -> Maybe TxId
payment_hash :: BS.ByteString -> Maybe PaymentHash
payment_preimage :: BS.ByteString -> Maybe PaymentPreimage
per_commitment_secret :: BS.ByteString -> Maybe PerCommitmentSecret

-- 48-bit range
commitment_number :: Word64 -> Maybe CommitmentNumber
```

Implementation pattern:

```haskell
-- | Parse a 33-byte compressed public key.
--
-- Returns Nothing if the input is not exactly 33 bytes.
--
-- >>> pubkey (BS.replicate 33 0x02)
-- Just (Pubkey ...)
-- >>> pubkey (BS.replicate 32 0x02)
-- Nothing
pubkey :: BS.ByteString -> Maybe Pubkey
pubkey bs
  | BS.length bs == 33 = Just (Pubkey bs)
  | otherwise = Nothing
{-# INLINE pubkey #-}
```

### 2. Update Types.hs exports

Add the smart constructors to the export list, grouped with their
corresponding types:

```haskell
    -- * Keys and points
  , Pubkey(..)
  , pubkey
  , Seckey(..)
  , seckey
  , Point(..)
  , point
  ...
```

### 3. Update BOLT3.hs re-exports

Add the new smart constructors to the main module's export list:

```haskell
    -- ** Keys and points
  , Pubkey(..)
  , pubkey
  , Seckey(..)
  , seckey
  , Point(..)
  , point
  ...
```

### 4. Add tests for smart constructors

Add test cases to `test/Main.hs`:

```haskell
smartConstructorTests :: TestTree
smartConstructorTests = testGroup "Smart constructors" [
    testCase "pubkey accepts 33 bytes" $ do
      let bs = BS.replicate 33 0x02
      isJust (pubkey bs) @?= True
  , testCase "pubkey rejects 32 bytes" $ do
      let bs = BS.replicate 32 0x02
      isNothing (pubkey bs) @?= True
  , testCase "commitment_number accepts 2^48-1" $ do
      isJust (commitment_number 281474976710655) @?= True
  , testCase "commitment_number rejects 2^48" $ do
      isNothing (commitment_number 281474976710656) @?= True
  ...
  ]
```

### 5. Verify build and tests pass

```bash
nix develop -c cabal build all
nix develop -c cabal test
```

## Files Modified

- `lib/Lightning/Protocol/BOLT3/Types.hs`
- `lib/Lightning/Protocol/BOLT3.hs`
- `test/Main.hs`

## Commit Message

```
Add smart constructors for type-safe parsing

- pubkey/point: validate 33-byte compressed EC points
- seckey/txid/payment_hash/payment_preimage: validate 32-byte values
- per_commitment_secret: validate 32-byte secrets
- commitment_number: validate 48-bit range

Raw constructors remain exported for internal use where size is
already guaranteed by construction.
```
