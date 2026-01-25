{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Encode
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Serialization for BOLT #3 transactions and scripts.
--
-- Provides Bitcoin transaction serialization in both standard SegWit
-- format (with witness data) and the stripped format used for signing.
--
-- == Transaction Format (SegWit)
--
-- * version (4 bytes LE)
-- * marker (0x00) + flag (0x01)
-- * input count (varint)
-- * inputs: outpoint (32+4), scriptSig length (varint), scriptSig, sequence
-- * output count (varint)
-- * outputs: value (8 LE), scriptPubKey length (varint), scriptPubKey
-- * witness data (for each input)
-- * locktime (4 bytes LE)

module Lightning.Protocol.BOLT3.Encode (
    -- * Transaction serialization
    encode_tx
  , encode_htlc_tx
  , encode_closing_tx
  , encode_tx_for_signing

    -- * Witness serialization
  , encode_witness
  , encode_funding_witness

    -- * Primitive encoding
  , encode_varint
  , encode_le32
  , encode_le64
  , encode_outpoint
  , encode_output
  ) where

import Data.Word (Word32, Word64)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Lightning.Protocol.BOLT3.Types
import Lightning.Protocol.BOLT3.Tx

-- primitive encoding ----------------------------------------------------------

-- | Encode a 32-bit value in little-endian format.
--
-- >>> encode_le32 0x12345678
-- "\x78\x56\x34\x12"
encode_le32 :: Word32 -> BS.ByteString
encode_le32 = BSL.toStrict . BSB.toLazyByteString . BSB.word32LE
{-# INLINE encode_le32 #-}

-- | Encode a 64-bit value in little-endian format.
--
-- >>> encode_le64 0x123456789ABCDEF0
-- "\xF0\xDE\xBC\x9A\x78\x56\x34\x12"
encode_le64 :: Word64 -> BS.ByteString
encode_le64 = BSL.toStrict . BSB.toLazyByteString . BSB.word64LE
{-# INLINE encode_le64 #-}

-- | Encode a value as a Bitcoin varint (CompactSize).
--
-- Encoding scheme:
--
-- * 0-252: 1 byte
-- * 253-65535: 0xFD followed by 2 bytes LE
-- * 65536-4294967295: 0xFE followed by 4 bytes LE
-- * larger: 0xFF followed by 8 bytes LE
--
-- >>> encode_varint 100
-- "\x64"
-- >>> encode_varint 1000
-- "\xFD\xE8\x03"
encode_varint :: Word64 -> BS.ByteString
encode_varint !n
  | n < 0xFD = BS.singleton (fromIntegral n)
  | n <= 0xFFFF = BSL.toStrict $ BSB.toLazyByteString $
      BSB.word8 0xFD <> BSB.word16LE (fromIntegral n)
  | n <= 0xFFFFFFFF = BSL.toStrict $ BSB.toLazyByteString $
      BSB.word8 0xFE <> BSB.word32LE (fromIntegral n)
  | otherwise = BSL.toStrict $ BSB.toLazyByteString $
      BSB.word8 0xFF <> BSB.word64LE n
{-# INLINE encode_varint #-}

-- | Encode an outpoint (txid + output index).
--
-- Format: 32 bytes txid (already LE in TxId) + 4 bytes output index LE
--
-- >>> encode_outpoint (Outpoint txid 0)
-- <32-byte txid><4-byte index>
encode_outpoint :: Outpoint -> BS.ByteString
encode_outpoint !op = BSL.toStrict $ BSB.toLazyByteString $
  BSB.byteString (unTxId $ outpoint_txid op) <>
  BSB.word32LE (outpoint_index op)
{-# INLINE encode_outpoint #-}

-- | Encode a transaction output.
--
-- Format: 8 bytes value LE + varint scriptPubKey length + scriptPubKey
--
-- >>> encode_output (TxOutput (Satoshi 100000) script OutputToLocal)
-- <8-byte value><varint length><scriptPubKey>
encode_output :: TxOutput -> BS.ByteString
encode_output !out = BSL.toStrict $ BSB.toLazyByteString $
  let !script = unScript (txout_script out)
      !scriptLen = fromIntegral (BS.length script) :: Word64
  in BSB.word64LE (unSatoshi $ txout_value out) <>
     varint_builder scriptLen <>
     BSB.byteString script
{-# INLINE encode_output #-}

-- witness encoding ------------------------------------------------------------

-- | Encode a witness stack.
--
-- Format: varint item count + (varint length + data) for each item
--
-- >>> encode_witness (Witness [sig, pubkey])
-- <varint 2><varint sigLen><sig><varint pkLen><pubkey>
encode_witness :: Witness -> BS.ByteString
encode_witness (Witness !items) = BSL.toStrict $ BSB.toLazyByteString $
  let !count = fromIntegral (length items) :: Word64
  in varint_builder count <> mconcat (map encode_witness_item items)
{-# INLINE encode_witness #-}

-- | Encode a single witness stack item.
encode_witness_item :: BS.ByteString -> BSB.Builder
encode_witness_item !bs =
  let !len = fromIntegral (BS.length bs) :: Word64
  in varint_builder len <> BSB.byteString bs
{-# INLINE encode_witness_item #-}

-- | Encode a funding witness (2-of-2 multisig).
--
-- The witness stack is: @0 <sig1> <sig2> <witnessScript>@
--
-- Signatures must be ordered to match pubkey order in the funding script.
--
-- >>> encode_funding_witness sig1 sig2 fundingScript
-- <witness with 4 items: empty, sig1, sig2, script>
encode_funding_witness
  :: BS.ByteString  -- ^ Signature for pubkey1 (lexicographically lesser)
  -> BS.ByteString  -- ^ Signature for pubkey2 (lexicographically greater)
  -> Script         -- ^ The funding witness script
  -> BS.ByteString
encode_funding_witness !sig1 !sig2 (Script !witnessScript) =
  BSL.toStrict $ BSB.toLazyByteString $
    varint_builder 4 <>
    encode_witness_item BS.empty <>
    encode_witness_item sig1 <>
    encode_witness_item sig2 <>
    encode_witness_item witnessScript
{-# INLINE encode_funding_witness #-}

-- transaction encoding --------------------------------------------------------

-- | Encode a commitment transaction (SegWit format with witness).
--
-- SegWit format:
--
-- * version (4 bytes LE)
-- * marker (0x00)
-- * flag (0x01)
-- * input count (varint)
-- * inputs
-- * output count (varint)
-- * outputs
-- * witness data
-- * locktime (4 bytes LE)
--
-- Note: The witness is empty (just count=0) since the commitment tx
-- spending the funding output requires external signatures.
encode_tx :: CommitmentTx -> BS.ByteString
encode_tx !tx = BSL.toStrict $ BSB.toLazyByteString $
  -- Version
  BSB.word32LE (ctx_version tx) <>
  -- SegWit marker and flag
  BSB.word8 0x00 <>
  BSB.word8 0x01 <>
  -- Input count (always 1 for commitment tx)
  varint_builder 1 <>
  -- Input: outpoint + empty scriptSig + sequence
  BSB.byteString (encode_outpoint (ctx_input_outpoint tx)) <>
  varint_builder 0 <>  -- scriptSig length (empty for SegWit)
  BSB.word32LE (unSequence $ ctx_input_sequence tx) <>
  -- Output count
  varint_builder (fromIntegral $ length $ ctx_outputs tx) <>
  -- Outputs
  mconcat (map (BSB.byteString . encode_output) (ctx_outputs tx)) <>
  -- Witness (empty stack for unsigned tx)
  varint_builder 0 <>
  -- Locktime
  BSB.word32LE (unLocktime $ ctx_locktime tx)

-- | Encode an HTLC transaction (SegWit format with witness).
--
-- HTLC transactions have a single input (the commitment tx HTLC output)
-- and a single output (the to_local-style delayed output).
encode_htlc_tx :: HTLCTx -> BS.ByteString
encode_htlc_tx !tx = BSL.toStrict $ BSB.toLazyByteString $
  -- Version
  BSB.word32LE (htx_version tx) <>
  -- SegWit marker and flag
  BSB.word8 0x00 <>
  BSB.word8 0x01 <>
  -- Input count (always 1)
  varint_builder 1 <>
  -- Input: outpoint + empty scriptSig + sequence
  BSB.byteString (encode_outpoint (htx_input_outpoint tx)) <>
  varint_builder 0 <>  -- scriptSig length (empty for SegWit)
  BSB.word32LE (unSequence $ htx_input_sequence tx) <>
  -- Output count (always 1)
  varint_builder 1 <>
  -- Output: value + scriptPubKey
  BSB.word64LE (unSatoshi $ htx_output_value tx) <>
  let !script = unScript (htx_output_script tx)
      !scriptLen = fromIntegral (BS.length script) :: Word64
  in varint_builder scriptLen <> BSB.byteString script <>
  -- Witness (empty stack for unsigned tx)
  varint_builder 0 <>
  -- Locktime
  BSB.word32LE (unLocktime $ htx_locktime tx)

-- | Encode a closing transaction (SegWit format with witness).
--
-- Closing transactions have a single input (the funding output) and
-- one or two outputs (to_local and/or to_remote).
encode_closing_tx :: ClosingTx -> BS.ByteString
encode_closing_tx !tx = BSL.toStrict $ BSB.toLazyByteString $
  -- Version
  BSB.word32LE (cltx_version tx) <>
  -- SegWit marker and flag
  BSB.word8 0x00 <>
  BSB.word8 0x01 <>
  -- Input count (always 1)
  varint_builder 1 <>
  -- Input: outpoint + empty scriptSig + sequence
  BSB.byteString (encode_outpoint (cltx_input_outpoint tx)) <>
  varint_builder 0 <>  -- scriptSig length (empty for SegWit)
  BSB.word32LE (unSequence $ cltx_input_sequence tx) <>
  -- Output count
  varint_builder (fromIntegral $ length $ cltx_outputs tx) <>
  -- Outputs
  mconcat (map (BSB.byteString . encode_output) (cltx_outputs tx)) <>
  -- Witness (empty stack for unsigned tx)
  varint_builder 0 <>
  -- Locktime
  BSB.word32LE (unLocktime $ cltx_locktime tx)

-- | Encode a commitment transaction for signing (stripped format).
--
-- The stripped format omits the SegWit marker, flag, and witness data.
-- This is the format used to compute the sighash for signing.
--
-- Format:
--
-- * version (4 bytes LE)
-- * input count (varint)
-- * inputs
-- * output count (varint)
-- * outputs
-- * locktime (4 bytes LE)
encode_tx_for_signing :: CommitmentTx -> BS.ByteString
encode_tx_for_signing !tx = BSL.toStrict $ BSB.toLazyByteString $
  -- Version
  BSB.word32LE (ctx_version tx) <>
  -- Input count (always 1 for commitment tx)
  varint_builder 1 <>
  -- Input: outpoint + empty scriptSig + sequence
  BSB.byteString (encode_outpoint (ctx_input_outpoint tx)) <>
  varint_builder 0 <>  -- scriptSig length (empty for SegWit)
  BSB.word32LE (unSequence $ ctx_input_sequence tx) <>
  -- Output count
  varint_builder (fromIntegral $ length $ ctx_outputs tx) <>
  -- Outputs
  mconcat (map (BSB.byteString . encode_output) (ctx_outputs tx)) <>
  -- Locktime
  BSB.word32LE (unLocktime $ ctx_locktime tx)

-- internal helpers ------------------------------------------------------------

-- | Build a varint directly to Builder.
varint_builder :: Word64 -> BSB.Builder
varint_builder !n
  | n < 0xFD = BSB.word8 (fromIntegral n)
  | n <= 0xFFFF = BSB.word8 0xFD <> BSB.word16LE (fromIntegral n)
  | n <= 0xFFFFFFFF = BSB.word8 0xFE <> BSB.word32LE (fromIntegral n)
  | otherwise = BSB.word8 0xFF <> BSB.word64LE n
{-# INLINE varint_builder #-}
