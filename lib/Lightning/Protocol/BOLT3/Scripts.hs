{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Scripts
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Scripts and witnesses for BOLT #3 transaction outputs.

module Lightning.Protocol.BOLT3.Scripts (
    -- * Funding output
    funding_script
  , funding_witness

    -- * to_local output
  , to_local_script
  , to_local_witness_spend
  , to_local_witness_revoke

    -- * to_remote output
  , to_remote_witness_script
  , to_remote_script_pubkey
  , to_remote_witness

    -- * Anchor outputs
  , anchor_script
  , anchor_witness_owner
  , anchor_witness_anyone

    -- * Offered HTLC outputs
  , offered_htlc_script
  , offered_htlc_witness_preimage
  , offered_htlc_witness_revoke

    -- * Received HTLC outputs
  , received_htlc_script
  , received_htlc_witness_timeout
  , received_htlc_witness_revoke

    -- * HTLC transaction inputs
  , htlc_success_witness
  , htlc_timeout_witness
  , remote_htlc_sighash

    -- * P2WSH
  , to_p2wsh
  ) where

import Bitcoin.Prim.Tx (Witness(..))
import Bitcoin.Prim.Tx.Sighash (SighashType(..))
import qualified Crypto.Hash.RIPEMD160 as RIPEMD160
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8, Word16, Word32)
import Lightning.Protocol.BOLT1 (Point, PaymentPreimage)
import qualified Lightning.Protocol.BOLT1 as BOLT1
import Lightning.Protocol.BOLT3.Types

-- opcodes --------------------------------------------------------------------

op_0, op_1, op_2, op_16 :: Word8
op_0  = 0x00
op_1  = 0x51
op_2  = 0x52
op_16 = 0x60

op_if, op_notif, op_else, op_endif, op_drop, op_dup, op_ifdup :: Word8
op_if    = 0x63
op_notif = 0x64
op_else  = 0x67
op_endif = 0x68
op_ifdup = 0x73
op_drop  = 0x75
op_dup   = 0x76

op_swap, op_size, op_equal, op_equalverify, op_hash160 :: Word8
op_swap        = 0x7c
op_size        = 0x82
op_equal       = 0x87
op_equalverify = 0x88
op_hash160     = 0xa9

op_checksig, op_checksigverify, op_checkmultisig :: Word8
op_checksig       = 0xac
op_checksigverify = 0xad
op_checkmultisig  = 0xae

op_checklocktimeverify, op_checksequenceverify :: Word8
op_checklocktimeverify = 0xb1
op_checksequenceverify = 0xb2

-- helpers --------------------------------------------------------------------

op :: Word8 -> BSB.Builder
op = BSB.word8
{-# INLINE op #-}

-- Push data of at most 75 bytes (all pushes here are keys, hashes or
-- short numbers).
push :: BS.ByteString -> BSB.Builder
push bs = BSB.word8 (fromIntegral (BS.length bs)) <> BSB.byteString bs
{-# INLINE push #-}

push_point :: Point -> BSB.Builder
push_point = push . BOLT1.un_point
{-# INLINE push_point #-}

-- Push a non-negative number, minimally encoded.
push_num :: Word32 -> BSB.Builder
push_num n
  | n == 0    = op op_0
  | n <= 16   = op (0x50 + fromIntegral n)
  | otherwise = push (BS.pack (signed (le n)))
  where
    -- little-endian magnitude
    le :: Word32 -> [Word8]
    le !x
      | x == 0    = []
      | otherwise = fromIntegral (x .&. 0xff) : le (x `shiftR` 8)

    -- append a sign byte if the top bit is set
    signed bs = case reverse bs of
      (h : _) | h .&. 0x80 /= 0 -> bs <> [0x00]
      _ -> bs

push_delay :: Word16 -> BSB.Builder
push_delay = push_num . fromIntegral
{-# INLINE push_delay #-}

script :: BSB.Builder -> Script
script = Script . BSL.toStrict . BSB.toLazyByteString
{-# INLINE script #-}

hash160 :: BS.ByteString -> BS.ByteString
hash160 = RIPEMD160.hash . SHA256.hash
{-# INLINE hash160 #-}

-- Append a witness script to a witness stack.
with_script :: [BS.ByteString] -> Script -> Witness
with_script items (Script s) = Witness (items <> [s])
{-# INLINE with_script #-}

-- P2WSH ----------------------------------------------------------------------

-- | The P2WSH scriptPubKey for a witness script:
--   @0 \<SHA256(script)\>@.
to_p2wsh :: Script -> Script
to_p2wsh (Script s) = script (op op_0 <> push (SHA256.hash s))

-- funding output -------------------------------------------------------------

-- | The funding output's witness script:
--
--   @2 \<pubkey1\> \<pubkey2\> 2 OP_CHECKMULTISIG@
--
--   where @pubkey1@ is the lexicographically lesser of the two funding
--   pubkeys. The argument order doesn't matter.
funding_script :: FundingPubkey -> FundingPubkey -> Script
funding_script (FundingPubkey a) (FundingPubkey b) =
  let (lo, hi) = if a <= b then (a, b) else (b, a)
  in  script $
           op op_2 <> push_point lo <> push_point hi
        <> op op_2 <> op op_checkmultisig

-- | The witness spending the funding output, given each party's
--   funding pubkey and signature (DER-encoded, with sighash byte):
--
--   @0 \<signature_for_pubkey1\> \<signature_for_pubkey2\> \<script\>@
--
--   The signatures are ordered to match the pubkeys in
--   'funding_script', so the argument order doesn't matter.
funding_witness
  :: FundingPubkey -> BS.ByteString
  -> FundingPubkey -> BS.ByteString
  -> Witness
funding_witness pa@(FundingPubkey a) sa pb@(FundingPubkey b) sb =
  let (s1, s2) = if a <= b then (sa, sb) else (sb, sa)
  in  with_script [BS.empty, s1, s2] (funding_script pa pb)

-- to_local output ------------------------------------------------------------

-- | The @to_local@ output's witness script:
--
--   @
--   OP_IF
--       \<revocationpubkey\>
--   OP_ELSE
--       \<to_self_delay\> OP_CHECKSEQUENCEVERIFY OP_DROP
--       \<local_delayedpubkey\>
--   OP_ENDIF
--   OP_CHECKSIG
--   @
--
--   HTLC-success and HTLC-timeout transaction outputs use the same
--   script.
to_local_script
  :: RevocationPubkey
  -> ToSelfDelay
  -> LocalDelayedPubkey
  -> Script
to_local_script
  (RevocationPubkey rev)
  (ToSelfDelay delay)
  (LocalDelayedPubkey delayed) = script $
       op op_if
    <> push_point rev
    <> op op_else
    <> push_delay delay
    <> op op_checksequenceverify
    <> op op_drop
    <> push_point delayed
    <> op op_endif
    <> op op_checksig

-- | The witness for the owner's delayed spend of a @to_local@ (or
--   HTLC transaction) output, given its witness script:
--
--   @\<local_delayedsig\> \<\> \<script\>@
--
--   The spending input's @nSequence@ must be @to_self_delay@.
to_local_witness_spend :: BS.ByteString -> Script -> Witness
to_local_witness_spend sig = with_script [sig, BS.empty]

-- | The witness for the revocation spend of a @to_local@ (or HTLC
--   transaction) output, given its witness script:
--
--   @\<revocation_sig\> 1 \<script\>@
to_local_witness_revoke :: BS.ByteString -> Script -> Witness
to_local_witness_revoke sig = with_script [sig, BS.singleton 0x01]

-- to_remote output -----------------------------------------------------------

-- | The @to_remote@ output's witness script under 'Anchors':
--
--   @\<remotepubkey\> OP_CHECKSIGVERIFY 1 OP_CHECKSEQUENCEVERIFY@
--
--   (Under 'StaticRemotekey' the output is P2WPKH, with no witness
--   script; see 'to_remote_script_pubkey'.)
to_remote_witness_script :: RemotePubkey -> Script
to_remote_witness_script (RemotePubkey pk) = script $
     push_point pk
  <> op op_checksigverify
  <> op op_1
  <> op op_checksequenceverify

-- | The @to_remote@ output's scriptPubKey: P2WPKH to @remotepubkey@
--   under 'StaticRemotekey', and P2WSH of 'to_remote_witness_script'
--   under 'Anchors'.
to_remote_script_pubkey :: RemotePubkey -> CommitmentFormat -> Script
to_remote_script_pubkey pk@(RemotePubkey p) fmt = case fmt of
  StaticRemotekey -> script (op op_0 <> push (hash160 (BOLT1.un_point p)))
  Anchors         -> to_p2wsh (to_remote_witness_script pk)

-- | The witness spending a @to_remote@ output: @\<remote_sig\>
--   \<remotepubkey\>@ (P2WPKH) under 'StaticRemotekey', and
--   @\<remote_sig\> \<script\>@ under 'Anchors', where the spending
--   input's @nSequence@ must be 1.
to_remote_witness
  :: BS.ByteString -> RemotePubkey -> CommitmentFormat -> Witness
to_remote_witness sig pk@(RemotePubkey p) fmt = case fmt of
  StaticRemotekey -> Witness [sig, BOLT1.un_point p]
  Anchors         -> with_script [sig] (to_remote_witness_script pk)

-- anchor outputs -------------------------------------------------------------

-- | An anchor output's witness script, for the funding pubkey of the
--   party it belongs to:
--
--   @
--   \<funding_pubkey\> OP_CHECKSIG OP_IFDUP
--   OP_NOTIF
--       OP_16 OP_CHECKSEQUENCEVERIFY
--   OP_ENDIF
--   @
anchor_script :: FundingPubkey -> Script
anchor_script (FundingPubkey pk) = script $
     push_point pk
  <> op op_checksig
  <> op op_ifdup
  <> op op_notif
  <> op op_16
  <> op op_checksequenceverify
  <> op op_endif

-- | The witness for an anchor's owner to spend it:
--   @\<sig\> \<script\>@.
anchor_witness_owner :: BS.ByteString -> FundingPubkey -> Witness
anchor_witness_owner sig pk = with_script [sig] (anchor_script pk)

-- | The witness for anyone to sweep an anchor, 16 blocks after the
--   commitment transaction confirms: @\<\> \<script\>@.
anchor_witness_anyone :: FundingPubkey -> Witness
anchor_witness_anyone pk = with_script [BS.empty] (anchor_script pk)

-- offered HTLC outputs -------------------------------------------------------

-- csv suffix for anchors HTLC scripts
htlc_csv :: CommitmentFormat -> BSB.Builder
htlc_csv fmt = case fmt of
  StaticRemotekey -> mempty
  Anchors         -> op op_1 <> op op_checksequenceverify <> op op_drop
{-# INLINE htlc_csv #-}

-- | An offered HTLC output's witness script:
--
--   @
--   OP_DUP OP_HASH160 \<RIPEMD160(SHA256(revocationpubkey))\> OP_EQUAL
--   OP_IF
--       OP_CHECKSIG
--   OP_ELSE
--       \<remote_htlcpubkey\> OP_SWAP OP_SIZE 32 OP_EQUAL
--       OP_NOTIF
--           OP_DROP 2 OP_SWAP \<local_htlcpubkey\> 2 OP_CHECKMULTISIG
--       OP_ELSE
--           OP_HASH160 \<RIPEMD160(payment_hash)\> OP_EQUALVERIFY
--           OP_CHECKSIG
--       OP_ENDIF
--   OP_ENDIF
--   @
--
--   Under 'Anchors', @1 OP_CHECKSEQUENCEVERIFY OP_DROP@ precedes the
--   final @OP_ENDIF@.
offered_htlc_script
  :: RevocationPubkey
  -> RemoteHtlcPubkey
  -> LocalHtlcPubkey
  -> BOLT1.PaymentHash
  -> CommitmentFormat
  -> Script
offered_htlc_script
  (RevocationPubkey rev)
  (RemoteHtlcPubkey remote)
  (LocalHtlcPubkey local)
  ph
  fmt = script $
       op op_dup <> op op_hash160
    <> push (hash160 (BOLT1.un_point rev))
    <> op op_equal
    <> op op_if
    <>   op op_checksig
    <> op op_else
    <>   push_point remote <> op op_swap <> op op_size
    <>   push (BS.singleton 32) <> op op_equal
    <>   op op_notif
    <>     op op_drop <> op op_2 <> op op_swap
    <>     push_point local <> op op_2 <> op op_checkmultisig
    <>   op op_else
    <>     op op_hash160
    <>     push (RIPEMD160.hash (BOLT1.un_payment_hash ph))
    <>     op op_equalverify <> op op_checksig
    <>   op op_endif
    <>   htlc_csv fmt
    <> op op_endif

-- | The witness for the remote party to claim an offered HTLC output
--   with the payment preimage, given its witness script:
--
--   @\<remotehtlcsig\> \<payment_preimage\> \<script\>@
--
--   Under 'Anchors' the spending input's @nSequence@ must be 1.
offered_htlc_witness_preimage
  :: BS.ByteString -> PaymentPreimage -> Script -> Witness
offered_htlc_witness_preimage sig pre =
  with_script [sig, BOLT1.un_payment_preimage pre]

-- | The witness for the revocation spend of an offered HTLC output,
--   given its witness script:
--
--   @\<revocation_sig\> \<revocationpubkey\> \<script\>@
offered_htlc_witness_revoke
  :: BS.ByteString -> RevocationPubkey -> Script -> Witness
offered_htlc_witness_revoke sig (RevocationPubkey rev) =
  with_script [sig, BOLT1.un_point rev]

-- received HTLC outputs ------------------------------------------------------

-- | A received HTLC output's witness script:
--
--   @
--   OP_DUP OP_HASH160 \<RIPEMD160(SHA256(revocationpubkey))\> OP_EQUAL
--   OP_IF
--       OP_CHECKSIG
--   OP_ELSE
--       \<remote_htlcpubkey\> OP_SWAP OP_SIZE 32 OP_EQUAL
--       OP_IF
--           OP_HASH160 \<RIPEMD160(payment_hash)\> OP_EQUALVERIFY
--           2 OP_SWAP \<local_htlcpubkey\> 2 OP_CHECKMULTISIG
--       OP_ELSE
--           OP_DROP \<cltv_expiry\> OP_CHECKLOCKTIMEVERIFY OP_DROP
--           OP_CHECKSIG
--       OP_ENDIF
--   OP_ENDIF
--   @
--
--   Under 'Anchors', @1 OP_CHECKSEQUENCEVERIFY OP_DROP@ precedes the
--   final @OP_ENDIF@.
received_htlc_script
  :: RevocationPubkey
  -> RemoteHtlcPubkey
  -> LocalHtlcPubkey
  -> BOLT1.PaymentHash
  -> CltvExpiry
  -> CommitmentFormat
  -> Script
received_htlc_script
  (RevocationPubkey rev)
  (RemoteHtlcPubkey remote)
  (LocalHtlcPubkey local)
  ph
  (CltvExpiry expiry)
  fmt = script $
       op op_dup <> op op_hash160
    <> push (hash160 (BOLT1.un_point rev))
    <> op op_equal
    <> op op_if
    <>   op op_checksig
    <> op op_else
    <>   push_point remote <> op op_swap <> op op_size
    <>   push (BS.singleton 32) <> op op_equal
    <>   op op_if
    <>     op op_hash160
    <>     push (RIPEMD160.hash (BOLT1.un_payment_hash ph))
    <>     op op_equalverify
    <>     op op_2 <> op op_swap <> push_point local
    <>     op op_2 <> op op_checkmultisig
    <>   op op_else
    <>     op op_drop <> push_num expiry
    <>     op op_checklocktimeverify <> op op_drop
    <>     op op_checksig
    <>   op op_endif
    <>   htlc_csv fmt
    <> op op_endif

-- | The witness for the remote party to time out a received HTLC
--   output, given its witness script:
--
--   @\<remotehtlcsig\> \<\> \<script\>@
--
--   Under 'Anchors' the spending input's @nSequence@ must be 1.
received_htlc_witness_timeout :: BS.ByteString -> Script -> Witness
received_htlc_witness_timeout sig = with_script [sig, BS.empty]

-- | The witness for the revocation spend of a received HTLC output,
--   given its witness script:
--
--   @\<revocation_sig\> \<revocationpubkey\> \<script\>@
received_htlc_witness_revoke
  :: BS.ByteString -> RevocationPubkey -> Script -> Witness
received_htlc_witness_revoke sig (RevocationPubkey rev) =
  with_script [sig, BOLT1.un_point rev]

-- HTLC transaction inputs ----------------------------------------------------

-- | The witness for an HTLC-success transaction's input, given both
--   HTLC signatures, the payment preimage and the received HTLC
--   output's witness script:
--
--   @0 \<remotehtlcsig\> \<localhtlcsig\> \<payment_preimage\> \<script\>@
--
--   See 'remote_htlc_sighash' for the signatures' sighash types.
htlc_success_witness
  :: BS.ByteString    -- ^ remotehtlcsig
  -> BS.ByteString    -- ^ localhtlcsig
  -> PaymentPreimage
  -> Script           -- ^ the received HTLC output's witness script
  -> Witness
htlc_success_witness remote local pre =
  with_script [BS.empty, remote, local, BOLT1.un_payment_preimage pre]

-- | The witness for an HTLC-timeout transaction's input, given both
--   HTLC signatures and the offered HTLC output's witness script:
--
--   @0 \<remotehtlcsig\> \<localhtlcsig\> \<\> \<script\>@
--
--   See 'remote_htlc_sighash' for the signatures' sighash types.
htlc_timeout_witness
  :: BS.ByteString    -- ^ remotehtlcsig
  -> BS.ByteString    -- ^ localhtlcsig
  -> Script           -- ^ the offered HTLC output's witness script
  -> Witness
htlc_timeout_witness remote local =
  with_script [BS.empty, remote, local, BS.empty]

-- | The sighash type of @remotehtlcsig@: the signature on an
--   HTLC-success or HTLC-timeout transaction that the commitment's
--   non-owner sends in @commitment_signed@.
--
--   Under 'Anchors' it is @SIGHASH_SINGLE|SIGHASH_ANYONECANPAY@, so
--   that the owner can add inputs and outputs to pay the (otherwise
--   zero) fee; under 'StaticRemotekey' it is @SIGHASH_ALL@. The
--   owner's own signature, @localhtlcsig@, always uses @SIGHASH_ALL@.
--
--   >>> remote_htlc_sighash Anchors
--   SIGHASH_SINGLE_ANYONECANPAY
remote_htlc_sighash :: CommitmentFormat -> SighashType
remote_htlc_sighash fmt = case fmt of
  StaticRemotekey -> SIGHASH_ALL
  Anchors         -> SIGHASH_SINGLE_ANYONECANPAY
