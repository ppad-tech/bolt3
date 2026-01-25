{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Decode
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Parsing for BOLT #3 transactions and scripts.
--
-- Decodes SegWit Bitcoin transactions from raw bytes.

module Lightning.Protocol.BOLT3.Decode (
    -- * Error types
    DecodeError(..)

    -- * Raw transaction type
  , RawTx(..)
  , RawInput(..)
  , RawOutput(..)

    -- * Transaction parsing
  , decode_tx

    -- * Witness parsing
  , decode_witness

    -- * Primitive decoding
  , decode_varint
  , decode_le32
  , decode_le64
  , decode_outpoint
  , decode_output
  ) where

import Data.Bits ((.|.), shiftL)
import Data.Word (Word8, Word32, Word64)
import qualified Data.ByteString as BS
import GHC.Generics (Generic)
import Lightning.Protocol.BOLT3.Types

-- error types -----------------------------------------------------------------

-- | Errors that can occur during transaction decoding.
data DecodeError
  = InsufficientBytes !Int !Int
    -- ^ Expected bytes, actual bytes available
  | InvalidMarker !Word8
    -- ^ Invalid SegWit marker byte (expected 0x00)
  | InvalidFlag !Word8
    -- ^ Invalid SegWit flag byte (expected 0x01)
  | InvalidVarint
    -- ^ Malformed varint encoding
  | EmptyInput
    -- ^ No bytes to decode
  deriving (Eq, Show, Generic)

-- raw transaction types -------------------------------------------------------

-- | A raw transaction input as parsed from bytes.
data RawInput = RawInput
  { ri_outpoint   :: !Outpoint
  , ri_script_sig :: !BS.ByteString
  , ri_sequence   :: !Sequence
  } deriving (Eq, Show, Generic)

-- | A raw transaction output as parsed from bytes.
data RawOutput = RawOutput
  { ro_value  :: !Satoshi
  , ro_script :: !Script
  } deriving (Eq, Show, Generic)

-- | A raw transaction as parsed from bytes.
--
-- Supports both legacy and SegWit transaction formats.
data RawTx = RawTx
  { rtx_version  :: {-# UNPACK #-} !Word32
  , rtx_inputs   :: ![RawInput]
  , rtx_outputs  :: ![RawOutput]
  , rtx_witness  :: ![[BS.ByteString]]
    -- ^ Witness stack for each input (empty list for legacy tx)
  , rtx_locktime :: !Locktime
  } deriving (Eq, Show, Generic)

-- primitive decoding ----------------------------------------------------------

-- | Decode a little-endian 32-bit integer.
--
-- >>> decode_le32 (BS.pack [0x01, 0x00, 0x00, 0x00])
-- Right (1, "")
decode_le32 :: BS.ByteString -> Either DecodeError (Word32, BS.ByteString)
decode_le32 !bs
  | BS.length bs < 4 = Left (InsufficientBytes 4 (BS.length bs))
  | otherwise =
      let !b0 = fromIntegral (BS.index bs 0)
          !b1 = fromIntegral (BS.index bs 1)
          !b2 = fromIntegral (BS.index bs 2)
          !b3 = fromIntegral (BS.index bs 3)
          !val = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16)
                     .|. (b3 `shiftL` 24)
          !rest = BS.drop 4 bs
      in Right (val, rest)
{-# INLINE decode_le32 #-}

-- | Decode a little-endian 64-bit integer.
--
-- >>> decode_le64 (BS.pack [0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00])
-- Right (1, "")
decode_le64 :: BS.ByteString -> Either DecodeError (Word64, BS.ByteString)
decode_le64 !bs
  | BS.length bs < 8 = Left (InsufficientBytes 8 (BS.length bs))
  | otherwise =
      let !b0 = fromIntegral (BS.index bs 0)
          !b1 = fromIntegral (BS.index bs 1)
          !b2 = fromIntegral (BS.index bs 2)
          !b3 = fromIntegral (BS.index bs 3)
          !b4 = fromIntegral (BS.index bs 4)
          !b5 = fromIntegral (BS.index bs 5)
          !b6 = fromIntegral (BS.index bs 6)
          !b7 = fromIntegral (BS.index bs 7)
          !val = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16)
                    .|. (b3 `shiftL` 24) .|. (b4 `shiftL` 32)
                    .|. (b5 `shiftL` 40) .|. (b6 `shiftL` 48)
                    .|. (b7 `shiftL` 56)
          !rest = BS.drop 8 bs
      in Right (val, rest)
{-# INLINE decode_le64 #-}

-- | Decode a Bitcoin varint (CompactSize).
--
-- Encoding:
-- * 0x00-0xFC: 1 byte
-- * 0xFD: 2 bytes little-endian follow
-- * 0xFE: 4 bytes little-endian follow
-- * 0xFF: 8 bytes little-endian follow
--
-- >>> decode_varint (BS.pack [0x01])
-- Right (1, "")
-- >>> decode_varint (BS.pack [0xfd, 0x00, 0x01])
-- Right (256, "")
decode_varint :: BS.ByteString -> Either DecodeError (Word64, BS.ByteString)
decode_varint !bs
  | BS.null bs = Left EmptyInput
  | otherwise =
      let !first = BS.index bs 0
          !rest = BS.drop 1 bs
      in case first of
           0xFD -> decode_varint_16 rest
           0xFE -> decode_varint_32 rest
           0xFF -> decode_le64 rest
           _    -> Right (fromIntegral first, rest)
{-# INLINE decode_varint #-}

-- | Decode a 16-bit varint payload.
decode_varint_16 :: BS.ByteString -> Either DecodeError (Word64, BS.ByteString)
decode_varint_16 !bs
  | BS.length bs < 2 = Left (InsufficientBytes 2 (BS.length bs))
  | otherwise =
      let !b0 = fromIntegral (BS.index bs 0) :: Word64
          !b1 = fromIntegral (BS.index bs 1) :: Word64
          !val = b0 .|. (b1 `shiftL` 8)
          !rest = BS.drop 2 bs
      in Right (val, rest)
{-# INLINE decode_varint_16 #-}

-- | Decode a 32-bit varint payload.
decode_varint_32 :: BS.ByteString -> Either DecodeError (Word64, BS.ByteString)
decode_varint_32 !bs
  | BS.length bs < 4 = Left (InsufficientBytes 4 (BS.length bs))
  | otherwise =
      let !b0 = fromIntegral (BS.index bs 0) :: Word64
          !b1 = fromIntegral (BS.index bs 1) :: Word64
          !b2 = fromIntegral (BS.index bs 2) :: Word64
          !b3 = fromIntegral (BS.index bs 3) :: Word64
          !val = b0 .|. (b1 `shiftL` 8) .|. (b2 `shiftL` 16)
                    .|. (b3 `shiftL` 24)
          !rest = BS.drop 4 bs
      in Right (val, rest)
{-# INLINE decode_varint_32 #-}

-- | Decode a transaction outpoint (txid + output index).
--
-- Format: 32 bytes txid (little-endian) + 4 bytes index (little-endian)
--
-- >>> let txid = BS.replicate 32 0
-- >>> let idx = BS.pack [0x01, 0x00, 0x00, 0x00]
-- >>> decode_outpoint (txid <> idx)
-- Right (Outpoint {outpoint_txid = ..., outpoint_index = 1}, "")
decode_outpoint
  :: BS.ByteString
  -> Either DecodeError (Outpoint, BS.ByteString)
decode_outpoint !bs
  | BS.length bs < 36 = Left (InsufficientBytes 36 (BS.length bs))
  | otherwise =
      let !txid = TxId (BS.take 32 bs)
          !rest1 = BS.drop 32 bs
      in case decode_le32 rest1 of
           Left err -> Left err
           Right (!idx, !rest2) ->
             let !outpoint = Outpoint txid idx
             in Right (outpoint, rest2)
{-# INLINE decode_outpoint #-}

-- | Decode a transaction output (value + scriptPubKey).
--
-- Format: 8 bytes value (little-endian) + varint script length + script
decode_output :: BS.ByteString -> Either DecodeError (RawOutput, BS.ByteString)
decode_output !bs = do
  (!value, !rest1) <- decode_le64 bs
  (!scriptLen, !rest2) <- decode_varint rest1
  let !len = fromIntegral scriptLen
  if BS.length rest2 < len
    then Left (InsufficientBytes len (BS.length rest2))
    else
      let !script = Script (BS.take len rest2)
          !rest3 = BS.drop len rest2
          !output = RawOutput (Satoshi value) script
      in Right (output, rest3)
{-# INLINE decode_output #-}

-- witness parsing -------------------------------------------------------------

-- | Decode a witness stack for one input.
--
-- Format: varint num_items + (varint length + data) for each item
decode_witness
  :: BS.ByteString
  -> Either DecodeError (Witness, BS.ByteString)
decode_witness !bs = do
  (!numItems, !rest1) <- decode_varint bs
  (!items, !rest2) <- decode_witness_items (fromIntegral numItems) rest1 []
  Right (Witness items, rest2)
{-# INLINE decode_witness #-}

-- | Decode witness items recursively.
decode_witness_items
  :: Int
  -> BS.ByteString
  -> [BS.ByteString]
  -> Either DecodeError ([BS.ByteString], BS.ByteString)
decode_witness_items 0 !bs !acc = Right (reverse acc, bs)
decode_witness_items !n !bs !acc = do
  (!itemLen, !rest1) <- decode_varint bs
  let !len = fromIntegral itemLen
  if BS.length rest1 < len
    then Left (InsufficientBytes len (BS.length rest1))
    else
      let !item = BS.take len rest1
          !rest2 = BS.drop len rest1
      in decode_witness_items (n - 1) rest2 (item : acc)

-- | Decode witness stacks for all inputs (internal, returns list).
decode_witness_stacks
  :: Int
  -> BS.ByteString
  -> [[BS.ByteString]]
  -> Either DecodeError ([[BS.ByteString]], BS.ByteString)
decode_witness_stacks 0 !bs !acc = Right (reverse acc, bs)
decode_witness_stacks !n !bs !acc = do
  (Witness !items, !rest) <- decode_witness bs
  decode_witness_stacks (n - 1) rest (items : acc)

-- transaction parsing ---------------------------------------------------------

-- | Decode a raw Bitcoin transaction from bytes.
--
-- Handles both legacy and SegWit transaction formats.
--
-- SegWit format:
-- * version (4 bytes LE)
-- * marker (0x00) + flag (0x01)
-- * input count (varint)
-- * inputs: outpoint (32+4), scriptSig length (varint), scriptSig, sequence
-- * output count (varint)
-- * outputs: value (8 LE), scriptPubKey length (varint), scriptPubKey
-- * witness data (for each input)
-- * locktime (4 bytes LE)
--
-- >>> decode_tx rawTxBytes
-- Right (RawTx {...})
decode_tx :: BS.ByteString -> Either DecodeError RawTx
decode_tx !bs = do
  -- Version (4 bytes LE)
  (!version, !rest1) <- decode_le32 bs

  -- Check for SegWit marker/flag
  let !hasWitness = BS.length rest1 >= 2 &&
                    BS.index rest1 0 == 0x00 &&
                    BS.index rest1 1 == 0x01

  if hasWitness
    then decode_tx_segwit version (BS.drop 2 rest1)
    else decode_tx_legacy version rest1
{-# INLINE decode_tx #-}

-- | Decode a SegWit transaction (after marker/flag consumed).
decode_tx_segwit
  :: Word32
  -> BS.ByteString
  -> Either DecodeError RawTx
decode_tx_segwit !version !bs = do
  -- Input count and inputs
  (!inputCount, !rest1) <- decode_varint bs
  (!inputs, !rest2) <- decode_inputs (fromIntegral inputCount) rest1 []

  -- Output count and outputs
  (!outputCount, !rest3) <- decode_varint rest2
  (!outputs, !rest4) <- decode_outputs (fromIntegral outputCount) rest3 []

  -- Witness data for each input
  (!witnesses, !rest5) <- decode_witness_stacks (length inputs) rest4 []

  -- Locktime (4 bytes LE)
  (!locktime, !_rest6) <- decode_le32 rest5

  Right RawTx
    { rtx_version  = version
    , rtx_inputs   = inputs
    , rtx_outputs  = outputs
    , rtx_witness  = witnesses
    , rtx_locktime = Locktime locktime
    }

-- | Decode a legacy (non-SegWit) transaction.
decode_tx_legacy
  :: Word32
  -> BS.ByteString
  -> Either DecodeError RawTx
decode_tx_legacy !version !bs = do
  -- Input count and inputs
  (!inputCount, !rest1) <- decode_varint bs
  (!inputs, !rest2) <- decode_inputs (fromIntegral inputCount) rest1 []

  -- Output count and outputs
  (!outputCount, !rest3) <- decode_varint rest2
  (!outputs, !rest4) <- decode_outputs (fromIntegral outputCount) rest3 []

  -- Locktime (4 bytes LE)
  (!locktime, !_rest5) <- decode_le32 rest4

  Right RawTx
    { rtx_version  = version
    , rtx_inputs   = inputs
    , rtx_outputs  = outputs
    , rtx_witness  = []
    , rtx_locktime = Locktime locktime
    }

-- | Decode transaction inputs recursively.
decode_inputs
  :: Int
  -> BS.ByteString
  -> [RawInput]
  -> Either DecodeError ([RawInput], BS.ByteString)
decode_inputs 0 !bs !acc = Right (reverse acc, bs)
decode_inputs !n !bs !acc = do
  (!input, !rest) <- decode_input bs
  decode_inputs (n - 1) rest (input : acc)

-- | Decode a single transaction input.
--
-- Format: outpoint (36 bytes) + scriptSig length (varint) + scriptSig +
--         sequence (4 bytes LE)
decode_input :: BS.ByteString -> Either DecodeError (RawInput, BS.ByteString)
decode_input !bs = do
  (!outpoint, !rest1) <- decode_outpoint bs
  (!scriptLen, !rest2) <- decode_varint rest1
  let !len = fromIntegral scriptLen
  if BS.length rest2 < len
    then Left (InsufficientBytes len (BS.length rest2))
    else do
      let !scriptSig = BS.take len rest2
          !rest3 = BS.drop len rest2
      (!seqNum, !rest4) <- decode_le32 rest3
      let !input = RawInput outpoint scriptSig (Sequence seqNum)
      Right (input, rest4)

-- | Decode transaction outputs recursively.
decode_outputs
  :: Int
  -> BS.ByteString
  -> [RawOutput]
  -> Either DecodeError ([RawOutput], BS.ByteString)
decode_outputs 0 !bs !acc = Right (reverse acc, bs)
decode_outputs !n !bs !acc = do
  (!output, !rest) <- decode_output bs
  decode_outputs (n - 1) rest (output : acc)
