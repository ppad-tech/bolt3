# ppad-bolt3

![](https://img.shields.io/badge/license-MIT-brightgreen)

A pure Haskell implementation of [BOLT #3][bolt3] (Lightning Network
Bitcoin transaction and script formats), including commitment
transactions, HTLC transactions, closing transactions, per-commitment
key derivation, and secret storage.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  >
  > import qualified Data.ByteString as BS
  > import qualified Data.ByteString.Base16 as B16
  > import Lightning.Protocol.BOLT3
  >
  > -- key derivation (BOLT #3 Appendix E test vector)
  > let basepoint = Point (B16.decodeLenient "036d6caac248af96f6afa7f904f550253a0f3ef3f5aa2fe6838a95b216691468e2")
  > let perCommitment = PerCommitmentPoint (Point (B16.decodeLenient "025f7117a78150fe2ef97db7cfc83bd57b2e2c0d0dd25eaf467a4a1c2a45ce1486"))
  > let Just (Pubkey derived) = derive_pubkey basepoint perCommitment
  > B16.encode derived
  "0235f2dbfaa89b57ec7b055afe29849ef7ddfeb1cefdb9ebdc43f5494984db29e5"
  >
  > -- secret generation (BOLT #3 Appendix D test vector)
  > let seed = BS.replicate 32 0xFF
  > B16.encode (generate_from_seed seed 281474976710655)
  "7cc854b54e3e0dcdb010d7a3fee464a9687be6e8db3be6854c475621e007a5dc"
  >
  > -- fee calculation
  > let feerate = FeeratePerKw 5000
  > let features = ChannelFeatures { cf_option_anchors = False }
  > commitment_fee feerate features 2
  Satoshi 5340
  >
  > -- HTLC trimming
  > let dust = DustLimit (Satoshi 546)
  > htlc_trim_threshold dust feerate features HTLCOffered
  Satoshi 3861
```

## Documentation

Haddocks are hosted at [docs.ppad.tech/bolt3][hadoc].

## Security

This library aims at the maximum security achievable in a
garbage-collected language under an optimizing compiler such as GHC.
If you discover any vulnerabilities, please disclose them via
security@ppad.tech.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal build
$ cabal test
$ cabal bench
```

[bolt3]: https://github.com/lightning/bolts/blob/master/03-transactions.md
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
[hadoc]: https://docs.ppad.tech/bolt3
