{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Keys
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Per-commitment key derivation per BOLT #3.
--
-- Implements key derivation formulas:
--
-- @
-- pubkey = basepoint + SHA256(per_commitment_point || basepoint) * G
-- revocationpubkey = revocation_basepoint * SHA256(revocation_basepoint
--                      || per_commitment_point)
--                  + per_commitment_point * SHA256(per_commitment_point
--                      || revocation_basepoint)
-- @

module Lightning.Protocol.BOLT3.Keys (
    -- * Per-commitment point derivation
    derive_per_commitment_point

    -- * Key derivation
  , derive_pubkey
  , derive_localpubkey
  , derive_local_htlcpubkey
  , derive_remote_htlcpubkey
  , derive_local_delayedpubkey
  , derive_remote_delayedpubkey

    -- * Revocation key derivation
  , derive_revocationpubkey

    -- * Per-commitment secret generation
  , generate_from_seed
  , derive_secret

    -- * Per-commitment secret storage
  , SecretStore(..)
  , SecretEntry(..)
  , empty_store
  , insert_secret
  , derive_old_secret

    -- * Commitment number obscuring
  , obscured_commitment_number
  ) where

import Data.Bits ((.&.), xor, shiftL, testBit, complementBit)
import qualified Data.ByteString as BS
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Crypto.Curve.Secp256k1 as S
import qualified Crypto.Hash.SHA256 as SHA256
import Lightning.Protocol.BOLT3.Types

-- Per-commitment point derivation ----------------------------------------

-- | Derive the per-commitment point from a per-commitment secret.
--
-- @per_commitment_point = per_commitment_secret * G@
--
-- >>> let secret = PerCommitmentSecret (BS.replicate 32 0x01)
-- >>> derive_per_commitment_point secret
-- Just (PerCommitmentPoint ...)
derive_per_commitment_point
  :: PerCommitmentSecret
  -> Maybe PerCommitmentPoint
derive_per_commitment_point (PerCommitmentSecret sec) = do
  sk <- S.parse_int256 sec
  pk <- S.derive_pub sk
  let !bs = S.serialize_point pk
  pure $! PerCommitmentPoint (Point bs)
{-# INLINE derive_per_commitment_point #-}

-- Key derivation ---------------------------------------------------------

-- | Derive a pubkey from a basepoint and per-commitment point.
--
-- @pubkey = basepoint + SHA256(per_commitment_point || basepoint) * G@
--
-- This is the general derivation formula used for localpubkey,
-- local_htlcpubkey, remote_htlcpubkey, local_delayedpubkey, and
-- remote_delayedpubkey.
--
-- >>> derive_pubkey basepoint per_commitment_point
-- Just (Pubkey ...)
derive_pubkey
  :: Point             -- ^ basepoint
  -> PerCommitmentPoint -- ^ per_commitment_point
  -> Maybe Pubkey
derive_pubkey (Point basepointBs) (PerCommitmentPoint (Point pcpBs)) = do
  basepoint <- S.parse_point basepointBs
  -- SHA256(per_commitment_point || basepoint)
  let !h = SHA256.hash (pcpBs <> basepointBs)
  -- Treat hash as scalar and multiply by G
  tweak <- S.parse_int256 h
  tweakPoint <- S.derive_pub tweak
  -- Add basepoint + tweak*G
  let !result = S.add basepoint tweakPoint
      !bs = S.serialize_point result
  pure $! Pubkey bs
{-# INLINE derive_pubkey #-}

-- | Derive localpubkey from payment_basepoint and per_commitment_point.
--
-- >>> derive_localpubkey payment_basepoint per_commitment_point
-- Just (LocalPubkey ...)
derive_localpubkey
  :: PaymentBasepoint
  -> PerCommitmentPoint
  -> Maybe LocalPubkey
derive_localpubkey (PaymentBasepoint pt) pcp =
  LocalPubkey <$> derive_pubkey pt pcp
{-# INLINE derive_localpubkey #-}

-- | Derive local_htlcpubkey from htlc_basepoint and per_commitment_point.
--
-- >>> derive_local_htlcpubkey htlc_basepoint per_commitment_point
-- Just (LocalHtlcPubkey ...)
derive_local_htlcpubkey
  :: HtlcBasepoint
  -> PerCommitmentPoint
  -> Maybe LocalHtlcPubkey
derive_local_htlcpubkey (HtlcBasepoint pt) pcp =
  LocalHtlcPubkey <$> derive_pubkey pt pcp
{-# INLINE derive_local_htlcpubkey #-}

-- | Derive remote_htlcpubkey from htlc_basepoint and per_commitment_point.
--
-- >>> derive_remote_htlcpubkey htlc_basepoint per_commitment_point
-- Just (RemoteHtlcPubkey ...)
derive_remote_htlcpubkey
  :: HtlcBasepoint
  -> PerCommitmentPoint
  -> Maybe RemoteHtlcPubkey
derive_remote_htlcpubkey (HtlcBasepoint pt) pcp =
  RemoteHtlcPubkey <$> derive_pubkey pt pcp
{-# INLINE derive_remote_htlcpubkey #-}

-- | Derive local_delayedpubkey from delayed_payment_basepoint and
-- per_commitment_point.
--
-- >>> derive_local_delayedpubkey delayed_payment_basepoint per_commitment_point
-- Just (LocalDelayedPubkey ...)
derive_local_delayedpubkey
  :: DelayedPaymentBasepoint
  -> PerCommitmentPoint
  -> Maybe LocalDelayedPubkey
derive_local_delayedpubkey (DelayedPaymentBasepoint pt) pcp =
  LocalDelayedPubkey <$> derive_pubkey pt pcp
{-# INLINE derive_local_delayedpubkey #-}

-- | Derive remote_delayedpubkey from delayed_payment_basepoint and
-- per_commitment_point.
--
-- >>> derive_remote_delayedpubkey delayed_payment_basepoint pcp
-- Just (RemoteDelayedPubkey ...)
derive_remote_delayedpubkey
  :: DelayedPaymentBasepoint
  -> PerCommitmentPoint
  -> Maybe RemoteDelayedPubkey
derive_remote_delayedpubkey (DelayedPaymentBasepoint pt) pcp =
  RemoteDelayedPubkey <$> derive_pubkey pt pcp
{-# INLINE derive_remote_delayedpubkey #-}

-- Revocation key derivation ----------------------------------------------

-- | Derive revocationpubkey from revocation_basepoint and
-- per_commitment_point.
--
-- @
-- revocationpubkey = revocation_basepoint
--                      * SHA256(revocation_basepoint || per_commitment_point)
--                  + per_commitment_point
--                      * SHA256(per_commitment_point || revocation_basepoint)
-- @
--
-- >>> derive_revocationpubkey revocation_basepoint per_commitment_point
-- Just (RevocationPubkey ...)
derive_revocationpubkey
  :: RevocationBasepoint
  -> PerCommitmentPoint
  -> Maybe RevocationPubkey
derive_revocationpubkey
  (RevocationBasepoint (Point rbpBs))
  (PerCommitmentPoint (Point pcpBs)) = do
    rbp <- S.parse_point rbpBs
    pcp <- S.parse_point pcpBs
    -- SHA256(revocation_basepoint || per_commitment_point)
    let !h1 = SHA256.hash (rbpBs <> pcpBs)
    -- SHA256(per_commitment_point || revocation_basepoint)
    let !h2 = SHA256.hash (pcpBs <> rbpBs)
    -- Multiply points by their respective scalars
    s1 <- S.parse_int256 h1
    s2 <- S.parse_int256 h2
    p1 <- S.mul rbp s1  -- revocation_basepoint * h1
    p2 <- S.mul pcp s2  -- per_commitment_point * h2
    -- Add the two points
    let !result = S.add p1 p2
        !bs = S.serialize_point result
    pure $! RevocationPubkey (Pubkey bs)
{-# INLINE derive_revocationpubkey #-}

-- Per-commitment secret generation ---------------------------------------

-- | Generate the I'th per-commitment secret from a seed.
--
-- Implements the generate_from_seed algorithm from BOLT #3:
--
-- @
-- generate_from_seed(seed, I):
--     P = seed
--     for B in 47 down to 0:
--         if B set in I:
--             flip(B) in P
--             P = SHA256(P)
--     return P
-- @
--
-- >>> generate_from_seed seed 281474976710655
-- <32-byte secret>
generate_from_seed
  :: BS.ByteString  -- ^ seed (32 bytes)
  -> Word64         -- ^ index I (max 2^48 - 1)
  -> BS.ByteString  -- ^ per-commitment secret (32 bytes)
generate_from_seed seed idx = go 47 seed where
  go :: Int -> BS.ByteString -> BS.ByteString
  go !b !p
    | b < 0 = p
    | testBit idx b =
        let !p' = flip_bit b p
            !p'' = SHA256.hash p'
        in  go (b - 1) p''
    | otherwise = go (b - 1) p
{-# INLINE generate_from_seed #-}

-- | Derive a secret from a base secret.
--
-- This is a generalization of generate_from_seed used for efficient
-- secret storage. Given a base secret whose index has bits..47 the same
-- as target index I, derive the I'th secret.
--
-- @
-- derive_secret(base, bits, I):
--     P = base
--     for B in bits - 1 down to 0:
--         if B set in I:
--             flip(B) in P
--             P = SHA256(P)
--     return P
-- @
derive_secret
  :: BS.ByteString  -- ^ base secret
  -> Int            -- ^ bits (number of trailing bits to process)
  -> Word64         -- ^ target index I
  -> BS.ByteString  -- ^ derived secret
derive_secret base bits idx = go (bits - 1) base where
  go :: Int -> BS.ByteString -> BS.ByteString
  go !b !p
    | b < 0 = p
    | testBit idx b =
        let !p' = flip_bit b p
            !p'' = SHA256.hash p'
        in  go (b - 1) p''
    | otherwise = go (b - 1) p
{-# INLINE derive_secret #-}

-- | Flip bit B in a 32-byte bytestring.
--
-- "flip(B)" alternates the (B mod 8) bit of the (B div 8) byte.
flip_bit :: Int -> BS.ByteString -> BS.ByteString
flip_bit b bs =
  let !byteIdx = b `div` 8
      !bitIdx = b `mod` 8
      !len = BS.length bs
  in  if byteIdx >= len
      then bs
      else
        let !prefix = BS.take byteIdx bs
            !byte = BS.index bs byteIdx
            !byte' = complementBit byte bitIdx
            !suffix = BS.drop (byteIdx + 1) bs
        in  prefix <> BS.singleton byte' <> suffix
{-# INLINE flip_bit #-}

-- Per-commitment secret storage ------------------------------------------

-- | Entry in the secret store: (bucket, index, secret).
data SecretEntry = SecretEntry
  { se_bucket :: {-# UNPACK #-} !Int
  , se_index  :: {-# UNPACK #-} !Word64
  , se_secret :: !BS.ByteString
  } deriving (Eq, Show, Generic)

-- | Compact storage for per-commitment secrets.
--
-- Stores up to 49 (value, index) pairs, allowing efficient derivation
-- of any previously-received secret. This is possible because for a
-- given secret on a 2^X boundary, all secrets up to the next 2^X
-- boundary can be derived from it.
newtype SecretStore = SecretStore { unSecretStore :: [SecretEntry] }
  deriving (Eq, Show, Generic)

-- | Empty secret store.
empty_store :: SecretStore
empty_store = SecretStore []
{-# INLINE empty_store #-}

-- | Determine which bucket to store a secret in based on its index.
--
-- Counts trailing zeros in the index. Returns 0-47 for normal indices,
-- or 48 if index is 0 (the seed).
where_to_put_secret :: Word64 -> Int
where_to_put_secret idx = go 0 where
  go !b
    | b > 47 = 48  -- index 0, this is the seed
    | testBit idx b = b
    | otherwise = go (b + 1)
{-# INLINE where_to_put_secret #-}

-- | Insert a secret into the store, validating against existing secrets.
--
-- Returns Nothing if the secret doesn't derive correctly from known
-- secrets (indicating the secrets weren't generated from the same seed).
--
-- >>> insert_secret secret 281474976710655 empty_store
-- Just (SecretStore ...)
insert_secret
  :: BS.ByteString  -- ^ secret (32 bytes)
  -> Word64         -- ^ index
  -> SecretStore    -- ^ current store
  -> Maybe SecretStore
insert_secret secret idx (SecretStore known) = do
  let !bucket = where_to_put_secret idx
  -- Validate: for each bucket < this bucket, check we can derive
  validated <- validateBuckets bucket known
  if validated
    then
      -- Remove entries at bucket >= this bucket, then insert
      let !known' = filter (\e -> se_bucket e < bucket) known
          !entry = SecretEntry bucket idx secret
      in  pure $! SecretStore (known' ++ [entry])
    else Nothing
  where
    validateBuckets :: Int -> [SecretEntry] -> Maybe Bool
    validateBuckets b entries = go entries where
      go [] = Just True
      go (SecretEntry entryBucket knownIdx knownSecret : rest)
        | entryBucket >= b = go rest  -- skip entries at higher buckets
        | otherwise =
            -- Check if we can derive the known secret from the new one
            let !derived = derive_secret secret b knownIdx
            in  if derived == knownSecret
                then go rest
                else Nothing
{-# INLINE insert_secret #-}

-- | Derive a previously-received secret from the store.
--
-- Iterates over known secrets to find one whose index is a prefix of
-- the target index, then derives the target secret from it.
--
-- >>> derive_old_secret 281474976710654 store
-- Just <32-byte secret>
derive_old_secret
  :: Word64       -- ^ target index
  -> SecretStore  -- ^ store
  -> Maybe BS.ByteString
derive_old_secret targetIdx (SecretStore known) = go known where
  go :: [SecretEntry] -> Maybe BS.ByteString
  go [] = Nothing
  go (SecretEntry bucket knownIdx knownSecret : rest) =
    -- Mask off the non-zero prefix of the index using the entry's bucket
    let !mask = complement ((1 `shiftL` bucket) - 1)
    in  if (targetIdx .&. mask) == knownIdx
        then Just $! derive_secret knownSecret bucket targetIdx
        else go rest

  complement :: Word64 -> Word64
  complement x = x `xor` 0xFFFFFFFFFFFFFFFF
{-# INLINE derive_old_secret #-}

-- Commitment number obscuring --------------------------------------------

-- | Calculate the obscured commitment number.
--
-- The 48-bit commitment number is obscured by XOR with the lower 48 bits
-- of SHA256(payment_basepoint from open_channel
--         || payment_basepoint from accept_channel).
--
-- >>> obscured_commitment_number local_payment_bp remote_payment_bp cn
-- <obscured value>
obscured_commitment_number
  :: PaymentBasepoint   -- ^ opener's payment_basepoint
  -> PaymentBasepoint   -- ^ accepter's payment_basepoint
  -> CommitmentNumber   -- ^ commitment number (48-bit)
  -> Word64             -- ^ obscured commitment number
obscured_commitment_number
  (PaymentBasepoint (Point openerBs))
  (PaymentBasepoint (Point accepterBs))
  (CommitmentNumber cn) =
    let !h = SHA256.hash (openerBs <> accepterBs)
        -- Extract lower 48 bits (6 bytes) from the hash
        !lower48 = extractLower48 h
        -- Mask commitment number to 48 bits
        !cn48 = cn .&. 0xFFFFFFFFFFFF
    in  cn48 `xor` lower48
{-# INLINE obscured_commitment_number #-}

-- | Extract lower 48 bits from a 32-byte hash.
--
-- Takes bytes 26-31 (last 6 bytes) and interprets as big-endian Word64.
extractLower48 :: BS.ByteString -> Word64
extractLower48 h =
  let !b0 = fromIntegral (BS.index h 26) `shiftL` 40
      !b1 = fromIntegral (BS.index h 27) `shiftL` 32
      !b2 = fromIntegral (BS.index h 28) `shiftL` 24
      !b3 = fromIntegral (BS.index h 29) `shiftL` 16
      !b4 = fromIntegral (BS.index h 30) `shiftL` 8
      !b5 = fromIntegral (BS.index h 31)
  in  b0 + b1 + b2 + b3 + b4 + b5
{-# INLINE extractLower48 #-}
