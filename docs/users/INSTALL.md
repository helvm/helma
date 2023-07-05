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

For example:
```bash
cabal run helma -p examples/piet/pietcc/hi.png
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
# make exec
cabal new-exec helma -- -l BF -e -- '++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.'
cabal new-exec helma -- -l SQ -e -- '12 12 3 36 37 6 37 12 9 37 37 12 0 -1 15 38 36 18 12 12 21 53 37 24 37 12 27 37 37 30 36 12 -1 37 37 0 39 0 -1 72 101 108 108 111 44 32 87 111 114 108 100 33 10 53'
cabal new-exec helma -- -l LK examples/lazy/rst76/hello_world.lazy
cabal new-exec helma -- -l LK examples/lazy/rst76/v.lazy

# Start REPL.
make repl

# Generate documentation.
make haddock

# Analyze coverage.
make hpc
```

## Other

For more see [CONTRIBUTING](../developers/CONTRIBUTING.md).

## 🦄 🌈 ❤️ 💛 💚 💙 🤍 🖤
