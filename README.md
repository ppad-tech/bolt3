# ppad-bolt3

[![](https://img.shields.io/hackage/v/ppad-bolt3?color=blue)](https://hackage.haskell.org/package/ppad-bolt3)
![](https://img.shields.io/badge/license-MIT-brightgreen)
[![](https://img.shields.io/badge/haddock-bolt3-lightblue)](https://docs.ppad.tech/bolt3)

A pure Haskell implementation of [BOLT #3][bolt3], the Lightning
Network's Bitcoin transaction and script formats: commitment, HTLC and
closing transactions, their output scripts and witnesses,
per-commitment key derivation, and per-commitment secret generation and
storage.

Amounts, points and secrets use the types of [ppad-bolt1][bolt1], and
transactions convert to [ppad-tx][tx] for txids, sighashes and
serialization.

## Usage

A sample GHCi session:

```
  > :set -XOverloadedStrings
  > import qualified Bitcoin.Prim.Tx as BT
  > import qualified Data.ByteString as BS
  > import qualified Lightning.Protocol.BOLT1 as BOLT1
  > import Lightning.Protocol.BOLT3
  >
  > -- per-commitment secrets come from a seed; secrets aren't shown
  > let Just sd = seed (BS.replicate 32 0xff)
  > let Just cn = commitment_number 0
  > let i = commitment_secret_index cn
  > let secret = generate_from_seed sd i
  > secret
  PerCommitmentSecret <redacted>
  >
  > -- the peer keeps revealed secrets in compact storage
  > let Just store = insert_secret secret i empty_secret_store
  > fmap (== secret) (derive_old_secret i store)
  Just True
  >
  > -- the commitment's per-commitment point, and a private key for it
  > let Just pcp = derive_per_commitment_point secret
  > let Just base = seckey (BS.pack [1 .. 32])
  > derive_privkey base pcp
  Just (Seckey <redacted>)
  >
  > -- fees and HTLC trimming
  > commitment_fee (FeeratePerKw 5000) StaticRemotekey 2
  Satoshi 5340
  > let Just dust = fmap DustLimit (BOLT1.satoshi 546)
  > htlc_trim_threshold dust (FeeratePerKw 5000) StaticRemotekey HTLCOffered
  Satoshi 3861
  >
  > -- an option_simple_close transaction: the closer pays the fee, and
  > -- its OP_RETURN output has amount zero
  > let Just txid = BT.mkTxId (BS.replicate 32 0x01)
  > let Just [closer, closee] = traverse BOLT1.milli_satoshi [6000000, 4000000]
  > let Just fee = BOLT1.satoshi 500
  > let p2wpkh = Script . BS.append "\x00\x14" . BS.replicate 20
  :{
  ghci| let close = ClosingContext {
  ghci|         clc_funding_outpoint = BT.OutPoint txid 0
  ghci|       , clc_closer_msat      = closer
  ghci|       , clc_closee_msat      = closee
  ghci|       , clc_closer_script    = Script "\x6a"
  ghci|       , clc_closee_script    = p2wpkh 0x22
  ghci|       , clc_fee              = fee
  ghci|       , clc_locktime         = Locktime 0
  ghci|       , clc_outputs          = CloserAndCloseeOutputs
  ghci|       }
  ghci| :}
  > let Just tx = build_closing_tx close
  > fmap clo_value (cltx_outputs tx)
  Satoshi 0 :| [Satoshi 4000]
  > BS.length (encode_closing_tx tx)
  92
```

## Documentation

Haddocks (API documentation, etc.) are hosted at
[docs.ppad.tech/bolt3](https://docs.ppad.tech/bolt3).

## Specification

This library implements BOLT #3 as of lightning/bolts commit
[1aadb719][spec] (2026-09-21); a copy lives in `etc/`. Every test
vector in Appendices B through F is checked byte for byte.

Not implemented:

* `zero_fee_commitments` (version 3 commitment and HTLC transactions,
  the `shared_anchor` P2A output, and Appendix H). Only the
  `option_static_remotekey` and `option_anchors` commitment formats are
  supported.
* Funding transaction construction (including Appendix G's dual-funded
  vectors): the library provides the funding output's script and
  witness only.

The library builds unsigned transactions; signatures are made with
[ppad-secp256k1][secp] over sighashes from ppad-tx. Spending
transactions for on-chain resolution belong to BOLT #5.

## Performance

The aim is best-in-class performance for pure Haskell code. Run the
benchmarks with:

```
$ cabal bench
```

## Security

This library has received no security audit. Secret material (seeds,
secret keys, per-commitment secrets and their storage) is kept out of
`Show` output, and private-key derivation uses constant-time
arithmetic.

## Development

You'll require [Nix][nixos] with [flake][flake] support enabled. Enter a
development shell with:

```
$ nix develop
```

Then do e.g.:

```
$ cabal repl ppad-bolt3
```

to get a REPL for the main library.

[bolt3]: https://github.com/lightning/bolts/blob/master/03-transactions.md
[bolt1]: https://git.ppad.tech/bolt1
[tx]: https://git.ppad.tech/tx
[secp]: https://git.ppad.tech/secp256k1
[spec]: https://github.com/lightning/bolts/tree/1aadb719b4007c4cea0ba6e36b08c4fb53788dee
[nixos]: https://nixos.org/
[flake]: https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-flake.html
