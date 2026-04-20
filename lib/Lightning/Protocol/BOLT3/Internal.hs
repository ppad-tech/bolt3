{-# OPTIONS_HADDOCK hide #-}

-- |
-- Module: Lightning.Protocol.BOLT3.Internal
-- Copyright: (c) 2025 Jared Tobin
-- License: MIT
-- Maintainer: Jared Tobin <jared@ppad.tech>
--
-- Internal definitions for BOLT #3.
--
-- This module exports unsafe constructors that bypass
-- validation. Use only in tests or trusted internal code.

module Lightning.Protocol.BOLT3.Internal (
  -- * Unsafe constructors (bypass validation)
    unsafePubkey
  , unsafeSeckey
  , unsafeCommitmentNumber
  , unsafeSequence
  , unsafeLocktime
  ) where

import qualified Data.ByteString as BS
import Data.Word (Word32, Word64)
import Lightning.Protocol.BOLT3.Types

-- | Construct a 'Pubkey' without length validation.
--
-- For test use only.
unsafePubkey :: BS.ByteString -> Pubkey
unsafePubkey = Pubkey

-- | Construct a 'Seckey' without length validation.
--
-- For test use only.
unsafeSeckey :: BS.ByteString -> Seckey
unsafeSeckey = Seckey

-- | Construct a 'CommitmentNumber' without range
-- validation.
--
-- For test use only.
unsafeCommitmentNumber :: Word64 -> CommitmentNumber
unsafeCommitmentNumber = CommitmentNumber

-- | Construct a 'Sequence' directly.
--
-- For test use only.
unsafeSequence :: Word32 -> Sequence
unsafeSequence = Sequence

-- | Construct a 'Locktime' directly.
--
-- For test use only.
unsafeLocktime :: Word32 -> Locktime
unsafeLocktime = Locktime
