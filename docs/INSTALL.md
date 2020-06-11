# 🏗️ INSTALL

How to download, test and run.

## Download

You need a client of `git`:
```bash
git clone https://github.com/helvm/helma.git
cd helma
```

## Compile

To compile you need `cabal`:
```bash
cabal update
cabal new-clean && cabal new-build && cabal new-test
```

## Run

You can run Helpa by `cabal` or directly:
```bash
cabal run helma file_to_interpret
dist-newstyle/build/x86_64-osx/ghc-8.10.1/helma-0.5.0.0/x/helma/build/helma/helma file_to_interpret
```

## Other

For more see [CONTRIBUTING](CONTRIBUTING.md).
