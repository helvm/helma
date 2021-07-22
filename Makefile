.PHONY: all bench build check clean configure fast haddock hlint main repl report run stan stylish test update

all: update fast bench

fast: main report

bench:
	rm -f helma-benchmark.tix
	cabal new-bench --jobs -f ghcoptions

build:
	cabal new-build --jobs --enable-profiling -f ghcoptions

check:
	cabal check

clean:
	cabal new-clean
	if test -d .cabal-sandbox; then cabal sandbox delete; fi
	if test -d .hpc; then rm -r .hpc; fi

configure:
	rm cabal.project.local*
	cabal configure --enable-benchmarks --enable-coverage --enable-tests -f ghcoptions

haddock:
	cabal new-haddock

hlint:
	./hlint.sh

main:
	make stylish configure check build test

repl:
	cabal new-repl lib:helma

report:
	make haddock stan hlint

run:
	cabal new-run --jobs helma

stan:
	./stan.sh

stylish:
	stylish-haskell -r -v -i hs

test:
	cabal new-test --jobs --test-show-details=streaming -f ghcoptions

update:
	cabal update
