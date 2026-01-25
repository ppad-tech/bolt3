{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Scripts
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Script templates for BOLT #3 transaction outputs.
--
-- Includes witness scripts for:
--
-- * Funding output (2-of-2 multisig)
-- * to_local output (revocable with CSV delay)
-- * to_remote output (P2WPKH or anchored)
-- * Anchor outputs
-- * Offered HTLC outputs
-- * Received HTLC outputs
-- * HTLC-timeout/success output (revocable with delay)

module Lightning.Protocol.BOLT3.Scripts (
    -- * Funding output
    funding_script
  , funding_witness

    -- * to_local output
  , to_local_script
  , to_local_witness_spend
  , to_local_witness_revoke

    -- * to_remote output
  , to_remote_script
  , to_remote_witness

    -- * Anchor outputs
  , anchor_script
  , anchor_witness_owner
  , anchor_witness_anyone

    -- * Offered HTLC output
  , offered_htlc_script
  , offered_htlc_witness_preimage
  , offered_htlc_witness_revoke

    -- * Received HTLC output
  , received_htlc_script
  , received_htlc_witness_timeout
  , received_htlc_witness_revoke

    -- * HTLC-timeout/success output (same as to_local)
  , htlc_output_script
  , htlc_output_witness_spend
  , htlc_output_witness_revoke

    -- * P2WSH helpers
  , to_p2wsh
  , witness_script_hash
  ) where

import Data.Bits ((.&.), shiftR)
import Data.Word (Word8, Word16, Word32)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.RIPEMD160 as RIPEMD160
import Lightning.Protocol.BOLT3.Types

-- opcodes ---------------------------------------------------------------------

-- | OP_0 / OP_FALSE (0x00)
op_0 :: Word8
op_0 = 0x00

-- | OP_PUSHDATA for 1-75 bytes just uses the length as opcode
-- For data <=75 bytes, opcode is just the length

-- | OP_IF (0x63)
op_if :: Word8
op_if = 0x63

-- | OP_NOTIF (0x64)
op_notif :: Word8
op_notif = 0x64

-- | OP_ELSE (0x67)
op_else :: Word8
op_else = 0x67

-- | OP_ENDIF (0x68)
op_endif :: Word8
op_endif = 0x68

-- | OP_DROP (0x75)
op_drop :: Word8
op_drop = 0x75

-- | OP_DUP (0x76)
op_dup :: Word8
op_dup = 0x76

-- | OP_SWAP (0x7c)
op_swap :: Word8
op_swap = 0x7c

-- | OP_SIZE (0x82)
op_size :: Word8
op_size = 0x82

-- | OP_EQUAL (0x87)
op_equal :: Word8
op_equal = 0x87

-- | OP_EQUALVERIFY (0x88)
op_equalverify :: Word8
op_equalverify = 0x88

-- | OP_IFDUP (0x73)
op_ifdup :: Word8
op_ifdup = 0x73

-- | OP_HASH160 (0xa9)
op_hash160 :: Word8
op_hash160 = 0xa9

-- | OP_CHECKSIG (0xac)
op_checksig :: Word8
op_checksig = 0xac

-- | OP_CHECKSIGVERIFY (0xad)
op_checksigverify :: Word8
op_checksigverify = 0xad

-- | OP_CHECKMULTISIG (0xae)
op_checkmultisig :: Word8
op_checkmultisig = 0xae

-- | OP_CHECKLOCKTIMEVERIFY (0xb1)
op_checklocktimeverify :: Word8
op_checklocktimeverify = 0xb1

-- | OP_CHECKSEQUENCEVERIFY (0xb2)
op_checksequenceverify :: Word8
op_checksequenceverify = 0xb2

-- | OP_1 (0x51)
op_1 :: Word8
op_1 = 0x51

-- | OP_2 (0x52)
op_2 :: Word8
op_2 = 0x52

-- | OP_16 (0x60)
op_16 :: Word8
op_16 = 0x60

-- helpers ---------------------------------------------------------------------

-- | Push a bytestring onto the stack (handles length encoding).
--
-- For data <= 75 bytes, the length itself is the opcode.
push_data :: BS.ByteString -> BSB.Builder
push_data !bs
  | len <= 75 = BSB.word8 (fromIntegral len) <> BSB.byteString bs
  | len <= 255 = BSB.word8 0x4c <> BSB.word8 (fromIntegral len)
                 <> BSB.byteString bs
  | len <= 65535 = BSB.word8 0x4d <> BSB.word16LE (fromIntegral len)
                   <> BSB.byteString bs
  | otherwise = BSB.word8 0x4e <> BSB.word32LE (fromIntegral len)
                <> BSB.byteString bs
  where
    !len = BS.length bs
{-# INLINE push_data #-}

-- | Encode a Word16 as minimal script number (for CSV delays).
push_csv_delay :: Word16 -> BSB.Builder
push_csv_delay !n
  | n == 0 = BSB.word8 op_0
  | n <= 16 = BSB.word8 (0x50 + fromIntegral n)
  | n <= 0x7f = push_data (BS.singleton (fromIntegral n))
  | n <= 0x7fff = push_data (BS.pack [lo, hi])
  | otherwise = push_data (BS.pack [lo, hi, 0x00])  -- need sign byte
  where
    !lo = fromIntegral (n .&. 0xff)
    !hi = fromIntegral ((n `shiftR` 8) .&. 0xff)
{-# INLINE push_csv_delay #-}

-- | Encode a Word32 as minimal script number (for CLTV).
push_cltv :: Word32 -> BSB.Builder
push_cltv !n
  | n == 0 = BSB.word8 op_0
  | n <= 16 = BSB.word8 (0x50 + fromIntegral n)
  | otherwise = push_data (encode_scriptnum n)
  where
    encode_scriptnum :: Word32 -> BS.ByteString
    encode_scriptnum 0 = BS.empty
    encode_scriptnum !v =
      let -- Build bytes little-endian (LSB first)
          go :: Word32 -> [Word8]
          go 0 = []
          go !x = fromIntegral (x .&. 0xff) : go (x `shiftR` 8)
          !bytes = go v
          -- If MSB has high bit set, need 0x00 suffix for positive numbers
          !result = case reverse bytes of
            [] -> bytes
            (msb:_) | msb .&. 0x80 /= 0 -> bytes ++ [0x00]
            _ -> bytes
      in BS.pack result
{-# INLINE push_cltv #-}

-- | Build script from builder.
build_script :: BSB.Builder -> Script
build_script = Script . BSL.toStrict . BSB.toLazyByteString
{-# INLINE build_script #-}

-- | HASH160 = RIPEMD160(SHA256(x))
hash160 :: BS.ByteString -> BS.ByteString
hash160 = RIPEMD160.hash . SHA256.hash
{-# INLINE hash160 #-}

-- P2WSH helpers ---------------------------------------------------------------

-- | Compute SHA256 hash of a witness script.
--
-- >>> witness_script_hash (Script "some_script")
-- <32-byte SHA256 hash>
witness_script_hash :: Script -> BS.ByteString
witness_script_hash (Script !s) = SHA256.hash s
{-# INLINE witness_script_hash #-}

-- | Convert a witness script to P2WSH scriptPubKey.
--
-- P2WSH format: OP_0 <32-byte-hash>
--
-- >>> to_p2wsh some_witness_script
-- Script "\x00\x20<32-byte-hash>"
to_p2wsh :: Script -> Script
to_p2wsh !script =
  let !h = witness_script_hash script
  in build_script (BSB.word8 op_0 <> push_data h)
{-# INLINE to_p2wsh #-}

-- funding output --------------------------------------------------------------

-- | Funding output witness script (2-of-2 multisig).
--
-- Script: @2 <pubkey1> <pubkey2> 2 OP_CHECKMULTISIG@
--
-- Where pubkey1 is lexicographically lesser.
--
-- >>> funding_script pk1 pk2
-- Script "R!<pk_lesser>!<pk_greater>R\xae"
funding_script :: FundingPubkey -> FundingPubkey -> Script
funding_script (FundingPubkey (Pubkey !pk1)) (FundingPubkey (Pubkey !pk2)) =
  let (!lesser, !greater) = if pk1 <= pk2 then (pk1, pk2) else (pk2, pk1)
  in build_script $
       BSB.word8 op_2
       <> push_data lesser
       <> push_data greater
       <> BSB.word8 op_2
       <> BSB.word8 op_checkmultisig

-- | Witness for spending funding output.
--
-- Witness: @0 <sig1> <sig2>@
--
-- Signatures ordered to match pubkey order in script.
--
-- >>> funding_witness sig1 sig2
-- Witness ["", sig1, sig2]
funding_witness :: BS.ByteString -> BS.ByteString -> Witness
funding_witness !sig1 !sig2 = Witness [BS.empty, sig1, sig2]

-- to_local output -------------------------------------------------------------

-- | to_local witness script (revocable with CSV delay).
--
-- Script:
--
-- @
-- OP_IF
--     <revocationpubkey>
-- OP_ELSE
--     <to_self_delay>
--     OP_CHECKSEQUENCEVERIFY
--     OP_DROP
--     <local_delayedpubkey>
-- OP_ENDIF
-- OP_CHECKSIG
-- @
--
-- >>> to_local_script revpk delay localpk
-- Script "c!<revpk>g<delay>\xb2u!<localpk>h\xac"
to_local_script
  :: RevocationPubkey
  -> ToSelfDelay
  -> LocalDelayedPubkey
  -> Script
to_local_script
  (RevocationPubkey (Pubkey !revpk))
  (ToSelfDelay !delay)
  (LocalDelayedPubkey (Pubkey !localpk)) =
    build_script $
      BSB.word8 op_if
      <> push_data revpk
      <> BSB.word8 op_else
      <> push_csv_delay delay
      <> BSB.word8 op_checksequenceverify
      <> BSB.word8 op_drop
      <> push_data localpk
      <> BSB.word8 op_endif
      <> BSB.word8 op_checksig

-- | Witness for delayed spend of to_local output.
--
-- Input nSequence must be set to to_self_delay.
--
-- Witness: @<local_delayedsig> <>@
--
-- >>> to_local_witness_spend sig
-- Witness [sig, ""]
to_local_witness_spend :: BS.ByteString -> Witness
to_local_witness_spend !sig = Witness [sig, BS.empty]

-- | Witness for revocation spend of to_local output.
--
-- Witness: @<revocation_sig> 1@
--
-- >>> to_local_witness_revoke sig
-- Witness [sig, "\x01"]
to_local_witness_revoke :: BS.ByteString -> Witness
to_local_witness_revoke !sig = Witness [sig, BS.singleton 0x01]

-- to_remote output ------------------------------------------------------------

-- | to_remote witness script.
--
-- With option_anchors:
--
-- @
-- <remotepubkey> OP_CHECKSIGVERIFY 1 OP_CHECKSEQUENCEVERIFY
-- @
--
-- Without option_anchors: P2WPKH (just the pubkey hash).
--
-- >>> to_remote_script pk (ChannelFeatures True)
-- Script "!<pk>\xadQ\xb2"
to_remote_script :: RemotePubkey -> ChannelFeatures -> Script
to_remote_script (RemotePubkey (Pubkey !pk)) !features
  | has_anchors features =
      -- Anchors: script with 1-block CSV
      build_script $
        push_data pk
        <> BSB.word8 op_checksigverify
        <> BSB.word8 op_1
        <> BSB.word8 op_checksequenceverify
  | otherwise =
      -- No anchors: P2WPKH (OP_0 <20-byte-hash>)
      let !h = hash160 pk
      in build_script (BSB.word8 op_0 <> push_data h)

-- | Witness for spending to_remote output.
--
-- With option_anchors (P2WSH), input nSequence must be 1.
-- Witness: @<remote_sig>@ (witness script appended by caller)
--
-- Without option_anchors (P2WPKH):
-- Witness: @<remote_sig> <remotepubkey>@
--
-- >>> to_remote_witness sig pk (ChannelFeatures False)
-- Witness [sig, pk]
to_remote_witness :: BS.ByteString -> RemotePubkey -> ChannelFeatures -> Witness
to_remote_witness !sig (RemotePubkey (Pubkey !pk)) !features
  | has_anchors features = Witness [sig]
  | otherwise = Witness [sig, pk]

-- anchor outputs --------------------------------------------------------------

-- | Anchor output witness script.
--
-- Script:
--
-- @
-- <funding_pubkey> OP_CHECKSIG OP_IFDUP
-- OP_NOTIF
--     OP_16 OP_CHECKSEQUENCEVERIFY
-- OP_ENDIF
-- @
--
-- >>> anchor_script fundpk
-- Script "!<fundpk>\xac\x73d`\xb2h"
anchor_script :: FundingPubkey -> Script
anchor_script (FundingPubkey (Pubkey !pk)) =
  build_script $
    push_data pk
    <> BSB.word8 op_checksig
    <> BSB.word8 op_ifdup
    <> BSB.word8 op_notif
    <> BSB.word8 op_16
    <> BSB.word8 op_checksequenceverify
    <> BSB.word8 op_endif

-- | Witness for owner to spend anchor output.
--
-- Witness: @<sig>@
--
-- >>> anchor_witness_owner sig
-- Witness [sig]
anchor_witness_owner :: BS.ByteString -> Witness
anchor_witness_owner !sig = Witness [sig]

-- | Witness for anyone to sweep anchor output after 16 blocks.
--
-- Witness: @<>@
--
-- >>> anchor_witness_anyone
-- Witness [""]
anchor_witness_anyone :: Witness
anchor_witness_anyone = Witness [BS.empty]

-- offered HTLC output ---------------------------------------------------------

-- | Offered HTLC witness script.
--
-- Without option_anchors:
--
-- @
-- OP_DUP OP_HASH160 <RIPEMD160(SHA256(revocationpubkey))> OP_EQUAL
-- OP_IF
--     OP_CHECKSIG
-- OP_ELSE
--     <remote_htlcpubkey> OP_SWAP OP_SIZE 32 OP_EQUAL
--     OP_NOTIF
--         OP_DROP 2 OP_SWAP <local_htlcpubkey> 2 OP_CHECKMULTISIG
--     OP_ELSE
--         OP_HASH160 <RIPEMD160(payment_hash)> OP_EQUALVERIFY
--         OP_CHECKSIG
--     OP_ENDIF
-- OP_ENDIF
-- @
--
-- With option_anchors, adds @1 OP_CHECKSEQUENCEVERIFY OP_DROP@ before
-- final OP_ENDIF.
offered_htlc_script
  :: RevocationPubkey
  -> RemoteHtlcPubkey
  -> LocalHtlcPubkey
  -> PaymentHash
  -> ChannelFeatures
  -> Script
offered_htlc_script
  (RevocationPubkey (Pubkey !revpk))
  (RemoteHtlcPubkey (Pubkey !remotepk))
  (LocalHtlcPubkey (Pubkey !localpk))
  (PaymentHash !ph)
  !features =
    let !revpk_hash = hash160 revpk
        !payment_hash160 = RIPEMD160.hash ph
        !csv_suffix = if has_anchors features
          then BSB.word8 op_1
               <> BSB.word8 op_checksequenceverify
               <> BSB.word8 op_drop
          else mempty
    in build_script $
         -- OP_DUP OP_HASH160 <revpk_hash> OP_EQUAL
         BSB.word8 op_dup
         <> BSB.word8 op_hash160
         <> push_data revpk_hash
         <> BSB.word8 op_equal
         -- OP_IF OP_CHECKSIG
         <> BSB.word8 op_if
         <> BSB.word8 op_checksig
         -- OP_ELSE
         <> BSB.word8 op_else
         -- <remote_htlcpubkey> OP_SWAP OP_SIZE 32 OP_EQUAL
         <> push_data remotepk
         <> BSB.word8 op_swap
         <> BSB.word8 op_size
         <> push_data (BS.singleton 32)
         <> BSB.word8 op_equal
         -- OP_NOTIF
         <> BSB.word8 op_notif
         -- OP_DROP 2 OP_SWAP <local_htlcpubkey> 2 OP_CHECKMULTISIG
         <> BSB.word8 op_drop
         <> BSB.word8 op_2
         <> BSB.word8 op_swap
         <> push_data localpk
         <> BSB.word8 op_2
         <> BSB.word8 op_checkmultisig
         -- OP_ELSE
         <> BSB.word8 op_else
         -- OP_HASH160 <payment_hash160> OP_EQUALVERIFY OP_CHECKSIG
         <> BSB.word8 op_hash160
         <> push_data payment_hash160
         <> BSB.word8 op_equalverify
         <> BSB.word8 op_checksig
         -- OP_ENDIF
         <> BSB.word8 op_endif
         -- CSV suffix for anchors
         <> csv_suffix
         -- OP_ENDIF
         <> BSB.word8 op_endif

-- | Witness for remote node to claim offered HTLC with preimage.
--
-- With option_anchors, input nSequence must be 1.
--
-- Witness: @<remotehtlcsig> <payment_preimage>@
--
-- >>> offered_htlc_witness_preimage sig preimage
-- Witness [sig, preimage]
offered_htlc_witness_preimage
  :: BS.ByteString -> PaymentPreimage -> Witness
offered_htlc_witness_preimage !sig (PaymentPreimage !preimage) =
  Witness [sig, preimage]

-- | Witness for revocation spend of offered HTLC.
--
-- Witness: @<revocation_sig> <revocationpubkey>@
--
-- >>> offered_htlc_witness_revoke sig revpk
-- Witness [sig, revpk]
offered_htlc_witness_revoke :: BS.ByteString -> Pubkey -> Witness
offered_htlc_witness_revoke !sig (Pubkey !revpk) = Witness [sig, revpk]

-- received HTLC output --------------------------------------------------------

-- | Received HTLC witness script.
--
-- Without option_anchors:
--
-- @
-- OP_DUP OP_HASH160 <RIPEMD160(SHA256(revocationpubkey))> OP_EQUAL
-- OP_IF
--     OP_CHECKSIG
-- OP_ELSE
--     <remote_htlcpubkey> OP_SWAP OP_SIZE 32 OP_EQUAL
--     OP_IF
--         OP_HASH160 <RIPEMD160(payment_hash)> OP_EQUALVERIFY
--         2 OP_SWAP <local_htlcpubkey> 2 OP_CHECKMULTISIG
--     OP_ELSE
--         OP_DROP <cltv_expiry> OP_CHECKLOCKTIMEVERIFY OP_DROP
--         OP_CHECKSIG
--     OP_ENDIF
-- OP_ENDIF
-- @
--
-- With option_anchors, adds @1 OP_CHECKSEQUENCEVERIFY OP_DROP@ before
-- final OP_ENDIF.
received_htlc_script
  :: RevocationPubkey
  -> RemoteHtlcPubkey
  -> LocalHtlcPubkey
  -> PaymentHash
  -> CltvExpiry
  -> ChannelFeatures
  -> Script
received_htlc_script
  (RevocationPubkey (Pubkey !revpk))
  (RemoteHtlcPubkey (Pubkey !remotepk))
  (LocalHtlcPubkey (Pubkey !localpk))
  (PaymentHash !ph)
  (CltvExpiry !expiry)
  !features =
    let !revpk_hash = hash160 revpk
        !payment_hash160 = RIPEMD160.hash ph
        !csv_suffix = if has_anchors features
          then BSB.word8 op_1
               <> BSB.word8 op_checksequenceverify
               <> BSB.word8 op_drop
          else mempty
    in build_script $
         -- OP_DUP OP_HASH160 <revpk_hash> OP_EQUAL
         BSB.word8 op_dup
         <> BSB.word8 op_hash160
         <> push_data revpk_hash
         <> BSB.word8 op_equal
         -- OP_IF OP_CHECKSIG
         <> BSB.word8 op_if
         <> BSB.word8 op_checksig
         -- OP_ELSE
         <> BSB.word8 op_else
         -- <remote_htlcpubkey> OP_SWAP OP_SIZE 32 OP_EQUAL
         <> push_data remotepk
         <> BSB.word8 op_swap
         <> BSB.word8 op_size
         <> push_data (BS.singleton 32)
         <> BSB.word8 op_equal
         -- OP_IF
         <> BSB.word8 op_if
         -- OP_HASH160 <payment_hash160> OP_EQUALVERIFY
         <> BSB.word8 op_hash160
         <> push_data payment_hash160
         <> BSB.word8 op_equalverify
         -- 2 OP_SWAP <local_htlcpubkey> 2 OP_CHECKMULTISIG
         <> BSB.word8 op_2
         <> BSB.word8 op_swap
         <> push_data localpk
         <> BSB.word8 op_2
         <> BSB.word8 op_checkmultisig
         -- OP_ELSE
         <> BSB.word8 op_else
         -- OP_DROP <cltv_expiry> OP_CHECKLOCKTIMEVERIFY OP_DROP OP_CHECKSIG
         <> BSB.word8 op_drop
         <> push_cltv expiry
         <> BSB.word8 op_checklocktimeverify
         <> BSB.word8 op_drop
         <> BSB.word8 op_checksig
         -- OP_ENDIF
         <> BSB.word8 op_endif
         -- CSV suffix for anchors
         <> csv_suffix
         -- OP_ENDIF
         <> BSB.word8 op_endif

-- | Witness for remote node to timeout received HTLC.
--
-- With option_anchors, input nSequence must be 1.
--
-- Witness: @<remotehtlcsig> <>@
--
-- >>> received_htlc_witness_timeout sig
-- Witness [sig, ""]
received_htlc_witness_timeout :: BS.ByteString -> Witness
received_htlc_witness_timeout !sig = Witness [sig, BS.empty]

-- | Witness for revocation spend of received HTLC.
--
-- Witness: @<revocation_sig> <revocationpubkey>@
--
-- >>> received_htlc_witness_revoke sig revpk
-- Witness [sig, revpk]
received_htlc_witness_revoke :: BS.ByteString -> Pubkey -> Witness
received_htlc_witness_revoke !sig (Pubkey !revpk) = Witness [sig, revpk]

-- HTLC-timeout/success output -------------------------------------------------

-- | HTLC output witness script (same structure as to_local).
--
-- Used for HTLC-timeout and HTLC-success transaction outputs.
--
-- Script:
--
-- @
-- OP_IF
--     <revocationpubkey>
-- OP_ELSE
--     <to_self_delay>
--     OP_CHECKSEQUENCEVERIFY
--     OP_DROP
--     <local_delayedpubkey>
-- OP_ENDIF
-- OP_CHECKSIG
-- @
htlc_output_script
  :: RevocationPubkey
  -> ToSelfDelay
  -> LocalDelayedPubkey
  -> Script
htlc_output_script = to_local_script

-- | Witness for delayed spend of HTLC output.
--
-- Input nSequence must be set to to_self_delay.
--
-- Witness: @<local_delayedsig> 0@
htlc_output_witness_spend :: BS.ByteString -> Witness
htlc_output_witness_spend = to_local_witness_spend

-- | Witness for revocation spend of HTLC output.
--
-- Witness: @<revocationsig> 1@
htlc_output_witness_revoke :: BS.ByteString -> Witness
htlc_output_witness_revoke = to_local_witness_revoke
