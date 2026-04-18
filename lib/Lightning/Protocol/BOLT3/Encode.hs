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
-- Delegates to ppad-tx for transaction encoding.

module Lightning.Protocol.BOLT3.Encode (
    -- * Transaction serialization
    encode_tx
  , encode_htlc_tx
  , encode_closing_tx
  , encode_tx_for_signing

    -- * Witness serialization
  , encode_witness
  , encode_funding_witness
  ) where

import qualified Bitcoin.Prim.Tx as BT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import Data.Word (Word64)
import Lightning.Protocol.BOLT3.Types
import Lightning.Protocol.BOLT3.Tx

-- transaction encoding --------------------------------------------------------

-- | Encode a commitment transaction (SegWit format).
--
-- Returns 'Nothing' if the transaction has no outputs.
encode_tx :: CommitmentTx -> Maybe BS.ByteString
encode_tx = fmap BT.to_bytes . commitment_to_tx

-- | Encode an HTLC transaction (SegWit format).
encode_htlc_tx :: HTLCTx -> BS.ByteString
encode_htlc_tx = BT.to_bytes . htlc_to_tx

-- | Encode a closing transaction (SegWit format).
--
-- Returns 'Nothing' if the transaction has no outputs.
encode_closing_tx :: ClosingTx -> Maybe BS.ByteString
encode_closing_tx = fmap BT.to_bytes . closing_to_tx

-- | Encode a commitment transaction for signing (stripped
-- format, no witness).
--
-- Returns 'Nothing' if the transaction has no outputs.
encode_tx_for_signing
  :: CommitmentTx -> Maybe BS.ByteString
encode_tx_for_signing =
  fmap BT.to_bytes_legacy . commitment_to_tx

-- witness encoding ------------------------------------------------------------

-- | Encode a witness stack.
--
-- Format: varint item count, then for each item:
-- varint length followed by item data.
encode_witness :: Witness -> BS.ByteString
encode_witness (Witness !items) =
  BT.to_strict $
       put_varint (fromIntegral (length items))
    <> foldMap put_item items
  where
    put_item :: BS.ByteString -> BSB.Builder
    put_item !bs =
         put_varint (fromIntegral (BS.length bs))
      <> BSB.byteString bs

-- | Encode a varint to a 'BSB.Builder'.
put_varint :: Word64 -> BSB.Builder
put_varint !n
  | n < 0xFD  = BSB.word8 (fromIntegral n)
  | n <= 0xFFFF =
      BSB.word8 0xFD <> BSB.word16LE (fromIntegral n)
  | n <= 0xFFFFFFFF =
      BSB.word8 0xFE <> BSB.word32LE (fromIntegral n)
  | otherwise =
      BSB.word8 0xFF <> BSB.word64LE n
{-# INLINE put_varint #-}

-- | Encode a funding witness (2-of-2 multisig).
--
-- The witness stack is: @0 <sig1> <sig2> <witnessScript>@
--
-- Signatures must be ordered to match pubkey order in the
-- funding script.
encode_funding_witness
  :: BS.ByteString  -- ^ Signature for lesser pubkey
  -> BS.ByteString  -- ^ Signature for greater pubkey
  -> Script         -- ^ The funding witness script
  -> BS.ByteString
encode_funding_witness !sig1 !sig2 (Script !ws) =
  encode_witness
    (Witness [BS.empty, sig1, sig2, ws])
