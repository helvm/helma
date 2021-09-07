# 🏗️ INSTALL

How to download, test and run.

## Download

You need a client of `git`:
```bash
git clone https://github.com/helvm/helma.git
cd helma
```

## Compile

To compile you need `cabal` and `make`:
```bash
make
```

## Run

You can run Helpa by `cabal` or directly:
```bash
cabal run helma file_to_interpret
```

or after build:
```bash
dist-newstyle/build/x86_64-linux/ghc-8.10.5/helma-0.6.10.0/x/helma/build/helma/helma file_to_interpret
```

## By make

```bash
# Update Cabal's list of packages.
cabal update

# Initialize a sandbox and install the package's dependencies.
make install

# Configure & build the package.
make configure
make build

# Test package.
make test

# Benchmark package.
make bench

# Run executable.
make run

# Start REPL.
make repl

# Generate documentation.
make haddock

# Analyze coverage.
make hpc
```

## Other

For more see [CONTRIBUTING](CONTRIBUTING.md).

## 🌈 ❤️ 💛 💚 💙 🤍 🖤 🦄
