{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Keys
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Key derivation, per-commitment secrets and commitment number
-- obscuring, per BOLT #3.

module Lightning.Protocol.BOLT3.Keys (
    -- * Per-commitment points
    derive_per_commitment_point
  , derive_per_commitment_point'

    -- * Public keys
  , derive_pubkey
  , derive_revocationpubkey
  , CommitmentKeys(..)
  , derive_commitment_keys
  , derive_commitment_keys'

    -- * Private keys
  , derive_privkey
  , derive_revocationprivkey

    -- * Per-commitment secrets
  , generate_from_seed
  , SecretStore
  , empty_secret_store
  , insert_secret
  , derive_old_secret
  , secret_store
  , un_secret_store

    -- * Commitment number obscuring
  , obscured_commitment_number
  ) where

import Control.DeepSeq (NFData(..))
import qualified Crypto.Curve.Secp256k1 as S
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bits ((.&.), (.|.), complement, complementBit, shiftL, shiftR,
                  testBit, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BU
import Data.Word (Word64)
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT1 (Point, PerCommitmentSecret)
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT3.Types

-- points ---------------------------------------------------------------------

-- Serialize a secp256k1 point, failing on the point at infinity.
to_point :: S.Projective -> Maybe Point
to_point p
  | p == S._CURVE_ZERO = Nothing
  | otherwise          = case BOLT1.point (S.serialize_point p) of
      Just pt -> Just pt
      Nothing -> internal_error
{-# INLINE to_point #-}

-- per-commitment points ------------------------------------------------------

-- | Derive the per-commitment point from a per-commitment secret:
--
--   @per_commitment_point = per_commitment_secret * G@
--
--   Fails if the secret is not a valid secp256k1 secret key.
--
--   >>> let Just s = BOLT1.per_commitment_secret (BS.pack [31, 30 .. 0])
--   >>> let Just (PerCommitmentPoint p) = derive_per_commitment_point s
--   >>> B16.encode (BOLT1.un_point p)
--   "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"
derive_per_commitment_point
  :: PerCommitmentSecret
  -> Maybe PerCommitmentPoint
derive_per_commitment_point s = do
  sk <- S.parse_int256 (BOLT1.un_per_commitment_secret s)
  pk <- S.derive_pub sk
  PerCommitmentPoint <$> to_point pk

-- | As 'derive_per_commitment_point', but uses a precomputed
--   'S.Context' to speed up the scalar multiplication.
derive_per_commitment_point'
  :: S.Context
  -> PerCommitmentSecret
  -> Maybe PerCommitmentPoint
derive_per_commitment_point' tex s = do
  sk <- S.parse_int256 (BOLT1.un_per_commitment_secret s)
  pk <- S.derive_pub' tex sk
  PerCommitmentPoint <$> to_point pk

-- public keys ----------------------------------------------------------------

-- | Derive a per-commitment public key from a basepoint:
--
--   @pubkey = basepoint + SHA256(per_commitment_point || basepoint) * G@
--
--   This gives @local_htlcpubkey@, @remote_htlcpubkey@,
--   @local_delayedpubkey@ and the like, depending on the basepoint;
--   'derive_commitment_keys' derives all of a commitment's keys at
--   once.
--
--   Fails if the basepoint is not on the curve.
--
--   >>> let Just s = BOLT1.per_commitment_secret (BS.pack [31, 30 .. 0])
--   >>> let Just pcp = derive_per_commitment_point s
--   >>> let Just b = BOLT1.per_commitment_secret (BS.pack [0 .. 31])
--   >>> let Just (PerCommitmentPoint bp) = derive_per_commitment_point b
--   >>> fmap (B16.encode . BOLT1.un_point) (derive_pubkey bp pcp)
--   Just "0235f2dbfaa89b57ec7b055afe29849ef7ddfeb1cefdb9ebdc43f5494984db29e5"
derive_pubkey :: Point -> PerCommitmentPoint -> Maybe Point
derive_pubkey = derive_pubkey_with mul_g
{-# INLINE derive_pubkey #-}

-- Multiply the generator by a 32-byte scalar.
mul_g :: BS.ByteString -> Maybe S.Projective
mul_g bs = S.derive_pub =<< S.parse_int256 bs
{-# INLINE mul_g #-}

-- As 'mul_g', using a precomputed context.
mul_g' :: S.Context -> BS.ByteString -> Maybe S.Projective
mul_g' tex bs = S.derive_pub' tex =<< S.parse_int256 bs
{-# INLINE mul_g' #-}

derive_pubkey_with
  :: (BS.ByteString -> Maybe S.Projective)
  -> Point
  -> PerCommitmentPoint
  -> Maybe Point
derive_pubkey_with mul bp (PerCommitmentPoint pcp) = do
  let !bp_bs = BOLT1.un_point bp
  base <- S.parse_point bp_bs
  t    <- mul (SHA256.hash (BOLT1.un_point pcp <> bp_bs))
  to_point (S.add base t)
{-# INLINE derive_pubkey_with #-}

-- | Derive a commitment's @revocationpubkey@ from the revocation
--   basepoint of the party that will hold the revocation key, and the
--   commitment owner's per-commitment point:
--
--   @
--   revocationpubkey =
--       revocation_basepoint
--         * SHA256(revocation_basepoint || per_commitment_point)
--     + per_commitment_point
--         * SHA256(per_commitment_point || revocation_basepoint)
--   @
--
--   Fails if either point is not on the curve.
--
--   >>> let Just s = BOLT1.per_commitment_secret (BS.pack [31, 30 .. 0])
--   >>> let Just pcp = derive_per_commitment_point s
--   >>> let Just b = BOLT1.per_commitment_secret (BS.pack [0 .. 31])
--   >>> let Just (PerCommitmentPoint p) = derive_per_commitment_point b
--   >>> let Just r = derive_revocationpubkey (RevocationBasepoint p) pcp
--   >>> (\(RevocationPubkey k) -> B16.encode (BOLT1.un_point k)) r
--   "02916e326636d19c33f13e8c0c3a03dd157f332f3e99c317c141dd865eb01f8ff0"
derive_revocationpubkey
  :: RevocationBasepoint
  -> PerCommitmentPoint
  -> Maybe RevocationPubkey
derive_revocationpubkey
  (RevocationBasepoint rbp)
  (PerCommitmentPoint pcp) = do
    let !rbp_bs = BOLT1.un_point rbp
        !pcp_bs = BOLT1.un_point pcp
    r  <- S.parse_point rbp_bs
    p  <- S.parse_point pcp_bs
    s1 <- S.parse_int256 (SHA256.hash (rbp_bs <> pcp_bs))
    s2 <- S.parse_int256 (SHA256.hash (pcp_bs <> rbp_bs))
    p1 <- S.mul r s1
    p2 <- S.mul p s2
    RevocationPubkey <$> to_point (S.add p1 p2)

-- | The keys a commitment transaction's scripts use.
data CommitmentKeys = CommitmentKeys
  { ck_revocation_pubkey :: !RevocationPubkey
  , ck_local_delayed     :: !LocalDelayedPubkey
  , ck_local_htlc        :: !LocalHtlcPubkey
  , ck_remote_htlc       :: !RemoteHtlcPubkey
  , ck_remote_payment    :: !RemotePubkey
  , ck_local_funding     :: !FundingPubkey
  , ck_remote_funding    :: !FundingPubkey
  } deriving (Eq, Show, Generic)

instance NFData CommitmentKeys

-- | Derive the keys for a commitment transaction from both parties'
--   basepoints and funding pubkeys, and the commitment owner's
--   per-commitment point.
--
--   The owner is the "local" party: its delayed and HTLC basepoints
--   give @local_delayedpubkey@ and @local_htlcpubkey@, while the other
--   party's revocation and HTLC basepoints give @revocationpubkey@ and
--   @remote_htlcpubkey@. As every supported 'CommitmentFormat' uses
--   @option_static_remotekey@, @remotepubkey@ is the other party's
--   @payment_basepoint@.
--
--   Fails if a basepoint it derives a key from, or the per-commitment
--   point, is not on the curve.
derive_commitment_keys
  :: Basepoints          -- ^ the owner's basepoints
  -> FundingPubkey       -- ^ the owner's funding pubkey
  -> Basepoints          -- ^ the other party's basepoints
  -> FundingPubkey       -- ^ the other party's funding pubkey
  -> PerCommitmentPoint  -- ^ the owner's per-commitment point
  -> Maybe CommitmentKeys
derive_commitment_keys = derive_commitment_keys_with mul_g

-- | As 'derive_commitment_keys', but uses a precomputed 'S.Context'
--   to speed up the scalar multiplications.
derive_commitment_keys'
  :: S.Context
  -> Basepoints
  -> FundingPubkey
  -> Basepoints
  -> FundingPubkey
  -> PerCommitmentPoint
  -> Maybe CommitmentKeys
derive_commitment_keys' tex = derive_commitment_keys_with (mul_g' tex)

derive_commitment_keys_with
  :: (BS.ByteString -> Maybe S.Projective)
  -> Basepoints
  -> FundingPubkey
  -> Basepoints
  -> FundingPubkey
  -> PerCommitmentPoint
  -> Maybe CommitmentKeys
derive_commitment_keys_with mul local local_fund remote remote_fund pcp = do
  let DelayedPaymentBasepoint local_delayed_bp = bp_delayed_payment local
      HtlcBasepoint local_htlc_bp = bp_htlc local
      HtlcBasepoint remote_htlc_bp = bp_htlc remote
      PaymentBasepoint remote_payment_bp = bp_payment remote
  revocation   <- derive_revocationpubkey (bp_revocation remote) pcp
  delayed      <- derive_pubkey_with mul local_delayed_bp pcp
  local_htlc   <- derive_pubkey_with mul local_htlc_bp pcp
  remote_htlc  <- derive_pubkey_with mul remote_htlc_bp pcp
  pure CommitmentKeys
    { ck_revocation_pubkey = revocation
    , ck_local_delayed     = LocalDelayedPubkey delayed
    , ck_local_htlc        = LocalHtlcPubkey local_htlc
    , ck_remote_htlc       = RemoteHtlcPubkey remote_htlc
    , ck_remote_payment    = RemotePubkey remote_payment_bp
    , ck_local_funding     = local_fund
    , ck_remote_funding    = remote_fund
    }

-- private keys ---------------------------------------------------------------

-- | Derive the private key for a per-commitment public key from its
--   basepoint secret:
--
--   @privkey = basepoint_secret + SHA256(per_commitment_point || basepoint)@
--
--   where @basepoint = basepoint_secret * G@. The result is the secret
--   key for @'derive_pubkey' basepoint per_commitment_point@.
--
--   The arithmetic on the secret is constant-time. Fails (with
--   negligible probability) if the result is zero.
--
--   >>> let Just s = BOLT1.per_commitment_secret (BS.pack [31, 30 .. 0])
--   >>> let Just pcp = derive_per_commitment_point s
--   >>> let Just k = seckey (BS.pack [0 .. 31])
--   >>> fmap (B16.encode . un_seckey) (derive_privkey k pcp)
--   Just "cbced912d3b21bf196a766651e436aff192362621ce317704ea2f75d87e7be0f"
derive_privkey :: Seckey -> PerCommitmentPoint -> Maybe Seckey
derive_privkey (Seckey sk_bs) (PerCommitmentPoint pcp) = do
  sk <- S.parse_int256 sk_bs
  bp <- to_point =<< S.derive_pub sk
  let !h = SHA256.hash (BOLT1.un_point pcp <> BOLT1.un_point bp)
      !r = add_mod_n (w4 sk_bs) (reduce_n (w4 h))
  to_seckey r

-- | Derive a commitment's @revocationprivkey@ from the revocation
--   basepoint secret and the commitment's (revealed) per-commitment
--   secret:
--
--   @
--   revocationprivkey =
--       revocation_basepoint_secret
--         * SHA256(revocation_basepoint || per_commitment_point)
--     + per_commitment_secret
--         * SHA256(per_commitment_point || revocation_basepoint)
--   @
--
--   The result is the secret key for the 'derive_revocationpubkey'
--   of the corresponding points.
--
--   The arithmetic on the secrets is constant-time. Fails if the
--   per-commitment secret is not a valid secret key, or (with
--   negligible probability) if the result is zero.
--
--   >>> let Just s = BOLT1.per_commitment_secret (BS.pack [31, 30 .. 0])
--   >>> let Just k = seckey (BS.pack [0 .. 31])
--   >>> fmap (B16.encode . un_seckey) (derive_revocationprivkey k s)
--   Just "d09ffff62ddb2297ab000cc85bcb4283fdeb6aa052affbc9dddcf33b61078110"
derive_revocationprivkey
  :: Seckey               -- ^ revocation basepoint secret
  -> PerCommitmentSecret  -- ^ per-commitment secret
  -> Maybe Seckey
derive_revocationprivkey (Seckey rbs_bs) pcs = do
  let !pcs_bs = BOLT1.un_per_commitment_secret pcs
  rbs <- S.parse_int256 rbs_bs
  ps  <- S.parse_int256 pcs_bs
  rbp <- to_point =<< S.derive_pub rbs
  pcp <- to_point =<< S.derive_pub ps
  let !rbp_bs = BOLT1.un_point rbp
      !pcp_bs = BOLT1.un_point pcp
      !h1 = reduce_n (w4 (SHA256.hash (rbp_bs <> pcp_bs)))
      !h2 = reduce_n (w4 (SHA256.hash (pcp_bs <> rbp_bs)))
      !r  = add_mod_n (mul_mod_n (w4 rbs_bs) h1) (mul_mod_n (w4 pcs_bs) h2)
  to_seckey r

-- constant-time arithmetic modulo the secp256k1 group order ------------------

-- A 256-bit integer as four 64-bit limbs, least significant first.
data W4 = W4
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64
  {-# UNPACK #-} !Word64

-- The secp256k1 group order n.
_N :: W4
_N = W4 0xBFD25E8CD0364141 0xBAAEDCE6AF48A03B
        0xFFFFFFFFFFFFFFFE 0xFFFFFFFFFFFFFFFF

-- Read 32 big-endian bytes (the length is checked by the caller).
w4 :: BS.ByteString -> W4
w4 bs = W4 (limb 24) (limb 16) (limb 8) (limb 0) where
  limb :: Int -> Word64
  limb o = go 0 0 where
    go !acc !i
      | i == 8    = acc
      | otherwise =
          go ((acc `shiftL` 8) .|. fromIntegral (BU.unsafeIndex bs (o + i)))
             (i + 1)

-- Write 32 big-endian bytes.
unw4 :: W4 -> BS.ByteString
unw4 (W4 a b c d) = BSL.toStrict . BSB.toLazyByteString $
  BSB.word64BE d <> BSB.word64BE c <> BSB.word64BE b <> BSB.word64BE a

-- A valid secret key from a result in [0, n), failing on zero.
to_seckey :: W4 -> Maybe Seckey
to_seckey r@(W4 a b c d)
  | (a .|. b .|. c .|. d) == 0 = Nothing
  | otherwise                  = Just (Seckey (unw4 r))

-- a + b + c, for a carry c in {0, 1}; returns the sum and carry out.
adc :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
adc a b c =
  let !s = a + b + c
      !o = ((a .&. b) .|. ((a .|. b) .&. complement s)) `shiftR` 63
  in  (s, o)
{-# INLINE adc #-}

-- a - b - c, for a borrow c in {0, 1}; returns the difference and
-- borrow out.
sbb :: Word64 -> Word64 -> Word64 -> (Word64, Word64)
sbb a b c =
  let !s = a - b - c
      !o = ((complement a .&. b) .|. (complement (a `xor` b) .&. s))
             `shiftR` 63
  in  (s, o)
{-# INLINE sbb #-}

add4 :: W4 -> W4 -> (W4, Word64)
add4 (W4 a0 a1 a2 a3) (W4 b0 b1 b2 b3) =
  let !(s0, c0) = adc a0 b0 0
      !(s1, c1) = adc a1 b1 c0
      !(s2, c2) = adc a2 b2 c1
      !(s3, c3) = adc a3 b3 c2
  in  (W4 s0 s1 s2 s3, c3)
{-# INLINE add4 #-}

sub4 :: W4 -> W4 -> (W4, Word64)
sub4 (W4 a0 a1 a2 a3) (W4 b0 b1 b2 b3) =
  let !(s0, c0) = sbb a0 b0 0
      !(s1, c1) = sbb a1 b1 c0
      !(s2, c2) = sbb a2 b2 c1
      !(s3, c3) = sbb a3 b3 c2
  in  (W4 s0 s1 s2 s3, c3)
{-# INLINE sub4 #-}

-- x if the bit is 1, y if it is 0.
select :: Word64 -> W4 -> W4 -> W4
select bit (W4 x0 x1 x2 x3) (W4 y0 y1 y2 y3) =
  let !m = negate bit
      f x y = (x .&. m) .|. (y .&. complement m)
  in  W4 (f x0 y0) (f x1 y1) (f x2 y2) (f x3 y3)
{-# INLINE select #-}

-- x mod n, for any 256-bit x (one subtraction suffices, as 2^256 < 2n).
reduce_n :: W4 -> W4
reduce_n x =
  let !(t, borrow) = sub4 x _N
  in  select borrow x t

-- a + b mod n, for a, b < n.
add_mod_n :: W4 -> W4 -> W4
add_mod_n a b =
  let !(s, carry)  = add4 a b
      !(t, borrow) = sub4 s _N
  in  select (carry .|. (borrow `xor` 1)) t s

-- a * b mod n, for a, b < n, by double-and-add over all 256 bits of b.
mul_mod_n :: W4 -> W4 -> W4
mul_mod_n a (W4 b0 b1 b2 b3) = go (255 :: Int) (W4 0 0 0 0) where
  go !i !acc
    | i < 0     = acc
    | otherwise =
        let !d   = add_mod_n acc acc
            !bit = (limb (i `shiftR` 6) `shiftR` (i .&. 63)) .&. 1
        in  go (i - 1) (select bit (add_mod_n d a) d)
  limb :: Int -> Word64
  limb j = case j of
    0 -> b0
    1 -> b1
    2 -> b2
    _ -> b3

-- per-commitment secrets -----------------------------------------------------

-- Wrap a 32-byte value as a per-commitment secret.
to_secret :: BS.ByteString -> PerCommitmentSecret
to_secret bs = case BOLT1.per_commitment_secret bs of
  Just s  -> s
  Nothing -> internal_error
{-# INLINE to_secret #-}

-- Flip bit b (< 256) of a 32-byte value: the (b mod 8) bit of the
-- (b div 8) byte.
flip_bit :: Int -> BS.ByteString -> BS.ByteString
flip_bit b bs =
  let !(pre, post) = BS.splitAt (b `shiftR` 3) bs
  in  case BS.uncons post of
        Just (h, t) -> pre <> BS.cons (complementBit h (b .&. 7)) t
        Nothing     -> bs
{-# INLINE flip_bit #-}

-- The spec's derive_secret: the secret for index i, from a base
-- secret whose index agrees with i in bits 47 down to 'bits'.
derive_secret :: BS.ByteString -> Int -> Word64 -> BS.ByteString
derive_secret base bits i = go (bits - 1) base where
  go !b !p
    | b < 0       = p
    | testBit i b = go (b - 1) (SHA256.hash (flip_bit b p))
    | otherwise   = go (b - 1) p

-- | Generate the per-commitment secret with a given index from a seed,
--   per BOLT #3's @generate_from_seed@.
--
--   >>> let Just s = seed (BS.replicate 32 0xff)
--   >>> let Just i = secret_index 281474976710655
--   >>> B16.encode (BOLT1.un_per_commitment_secret (generate_from_seed s i))
--   "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc"
generate_from_seed :: Seed -> SecretIndex -> PerCommitmentSecret
generate_from_seed (Seed s) (SecretIndex i) = to_secret (derive_secret s 48 i)

-- | Compact storage for the per-commitment secrets received from a
--   peer, per BOLT #3's "Efficient Per-commitment Secret Storage".
--
--   Holds at most 49 secrets, from which every secret received so far
--   can be derived. This is secret material: its 'Show' instance is
--   redacted, and it has no 'Eq' instance.
newtype SecretStore = SecretStore [Entry]

-- A stored secret: its bucket (the number of trailing zeros of its
-- index), index, and value. Kept in ascending bucket order.
data Entry = Entry
  {-# UNPACK #-} !Int
  {-# UNPACK #-} !Word64
  !BS.ByteString

instance Show SecretStore where
  showsPrec d _ = showParen (d > 10) $
    showString "SecretStore <redacted>"

instance NFData SecretStore where
  rnf (SecretStore es) = rnf_entries es where
    rnf_entries [] = ()
    rnf_entries (Entry _ _ s : rest) = rnf s `seq` rnf_entries rest

-- | The empty 'SecretStore'.
empty_secret_store :: SecretStore
empty_secret_store = SecretStore []

-- The bucket for an index: its number of trailing zeros (48 for 0).
bucket_of :: Word64 -> Int
bucket_of i = go 0 where
  go !b
    | b > 47      = 48
    | testBit i b = b
    | otherwise   = go (b + 1)

-- Whether a secret at index j in bucket b can derive index i.
can_derive :: Int -> Word64 -> Word64 -> Bool
can_derive b j i = i .&. complement ((1 `shiftL` b) - 1) == j
{-# INLINE can_derive #-}

-- | Add a newly received per-commitment secret to the store, per
--   BOLT #3's @insert_secret@.
--
--   Fails if a stored secret cannot be derived from the new one, i.e.
--   if the secrets were not generated from the same seed (or were not
--   received in order).
--
--   >>> let Just s = seed (BS.replicate 32 0xff)
--   >>> let Just i = secret_index 281474976710655
--   >>> let secret = generate_from_seed s i
--   >>> let Just st = insert_secret secret i empty_secret_store
--   >>> fmap (== secret) (derive_old_secret i st)
--   Just True
insert_secret
  :: PerCommitmentSecret
  -> SecretIndex
  -> SecretStore
  -> Maybe SecretStore
insert_secret pcs (SecretIndex i) (SecretStore known)
  | all derives known = Just $! SecretStore (put known)
  | otherwise         = Nothing
  where
    !s = BOLT1.un_per_commitment_secret pcs
    !b = bucket_of i
    !e = Entry b i s

    -- every secret in a lower bucket must derive from the new one
    derives (Entry kb ki ks)
      | kb < b    = to_secret (derive_secret s b ki) == to_secret ks
      | otherwise = True

    -- replace this bucket's entry, keeping ascending bucket order
    put [] = [e]
    put (k@(Entry kb _ _) : ks) = case compare kb b of
      LT -> k : put ks
      EQ -> e : ks
      GT -> e : k : ks

-- | Derive a previously received per-commitment secret from the
--   store. Fails if the index has not been received.
derive_old_secret
  :: SecretIndex
  -> SecretStore
  -> Maybe PerCommitmentSecret
derive_old_secret (SecretIndex i) (SecretStore known) = go known where
  go [] = Nothing
  go (Entry b j s : rest)
    | can_derive b j i = Just $! to_secret (derive_secret s b i)
    | otherwise        = go rest

-- | Rebuild a 'SecretStore' from its entries, e.g. as produced by
--   'un_secret_store'.
--
--   Fails if two entries share a bucket (i.e. their indices have the
--   same number of trailing zeros), or if an entry contradicts another
--   from which it can be derived.
secret_store
  :: [(SecretIndex, PerCommitmentSecret)]
  -> Maybe SecretStore
secret_store = go [] where
  go acc [] = Just (SecretStore acc)
  go acc ((SecretIndex i, pcs) : rest) = do
    let !e = Entry (bucket_of i) i (BOLT1.un_per_commitment_secret pcs)
    acc' <- place e acc
    go acc' rest

  -- insert in ascending bucket order, checking consistency with the
  -- entries already placed
  place e@(Entry b i s) es
    | all (consistent e) es = put es
    | otherwise             = Nothing
    where
      put [] = Just [e]
      put (k@(Entry kb _ _) : ks) = case compare kb b of
        LT -> (k :) <$> put ks
        EQ -> Nothing
        GT -> Just (e : k : ks)
      consistent _ (Entry kb ki ks)
        | kb > b && can_derive kb ki i =
            to_secret (derive_secret ks kb i) == to_secret s
        | kb < b && can_derive b i ki =
            to_secret (derive_secret s b ki) == to_secret ks
        | otherwise = True

-- | The entries of a 'SecretStore', for persistence; see
--   'secret_store'.
un_secret_store :: SecretStore -> [(SecretIndex, PerCommitmentSecret)]
un_secret_store (SecretStore es) =
  [ (SecretIndex i, to_secret s) | Entry _ i s <- es ]

-- commitment number obscuring ------------------------------------------------

-- | Obscure a commitment number, by XOR with the lower 48 bits of
--
--   @SHA256(payment_basepoint from open_channel
--           || payment_basepoint from accept_channel)@
--
--   As XOR is an involution, applying this to an obscured number
--   recovers the commitment number. ('build_commitment_tx' obscures
--   the commitment number itself.)
--
--   >>> let pay h = fmap PaymentBasepoint (BOLT1.point =<< B16.decode h)
--   >>> let o1 = "034f355bdcb7cc0af728ef3cceb9615d9068"
--   >>> let Just o = pay (o1 <> "4bb5b2ca5f859ab0f0b704075871aa")
--   >>> let a1 = "032c0b7cf95324a07d05398b240174dc0c2b"
--   >>> let Just a = pay (a1 <> "e444d96b159aa6c7f7b1e668680991")
--   >>> fmap (obscured_commitment_number o a) (commitment_number 42)
--   Just 48035859142974
obscured_commitment_number
  :: PaymentBasepoint   -- ^ opener's payment_basepoint
  -> PaymentBasepoint   -- ^ accepter's payment_basepoint
  -> CommitmentNumber
  -> Word64
obscured_commitment_number
  (PaymentBasepoint opener)
  (PaymentBasepoint accepter)
  cn =
    let !h = SHA256.hash (BOLT1.un_point opener <> BOLT1.un_point accepter)
        !mask = BS.foldl' (\acc x -> (acc `shiftL` 8) .|. fromIntegral x) 0
                  (BS.drop 26 h)
    in  un_commitment_number cn `xor` mask
