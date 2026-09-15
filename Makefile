.PHONY: all bench build check check-whitespace clean configure exec fast golden haddock hlint hpack install main output profile-clean profile-cost profile-heap profile-test profile-test-match repl report run sdist stan stylish test tix update

JOBS ?= 2

MATCH ?= Piet Interpreter Golden Tests

all: update fast

bench:
	rm -f helma-benchmark.tix
	cabal bench --jobs=$(JOBS) -f ghcoptions

build:
	cabal build --jobs=$(JOBS) -f ghcoptions

check:
	cabal check

check-whitespace:
	git check-whitespace

clean: profile-clean
	cabal clean
	if test -d .cabal-sandbox; then rm -rf .cabal-sandbox; fi
	if test -d .hpc; then rm -rf .hpc; fi
	if test -d .hie; then rm -rf .hie; fi

configure:
	rm -f cabal.project.local*
	cabal configure --enable-tests -f ghcoptions

exec:
	make tix
	cabal run --jobs=$(JOBS) helma

fast: main report sdist install

golden:
	if test -d .output/golden; then rm -r .output/golden; fi

haddock:
	cabal haddock

hlint:
	./hlint.sh

hpack:
	curl -sSL https://github.com/sol/hpack/raw/main/get-hpack.sh | bash

install:
	cabal install all --overwrite-policy=always

main:
	make stylish configure check build test

output:
	if test -d .output; then rm -r .output; fi


profile-test-match: profile-clean
	cabal run --jobs=$(JOBS) -f ghcoptions helma-test --enable-profiling --ghc-options="-fprof-late" -- -m "$(MATCH)" +RTS -p -i0.2 -s

profile-test: profile-clean
	cabal run --jobs=$(JOBS) -f ghcoptions helma-test --enable-profiling --ghc-options="-fprof-late" -- +RTS -p -i0.1 -s

profile-heap: profile-clean
	cabal run --jobs=$(JOBS) -f ghcoptions helma-test --enable-profiling --ghc-options="-fprof-late" -- +RTS -hy -l
	@if command -v hp2pretty >/dev/null 2>&1; then hp2pretty *.hp; elif command -v hp2ps >/dev/null 2>&1; then hp2ps -c *.hp; fi

profile-cost: profile-clean
	cabal run --jobs=$(JOBS) -f ghcoptions helma-test --enable-profiling --ghc-options="-fprof-late" -- +RTS -hc -l
	@if command -v hp2pretty >/dev/null 2>&1; then hp2pretty *.hp; elif command -v hp2ps >/dev/null 2>&1; then hp2ps -c *.hp; fi


profile-clean:
	rm -f *.prof *.hp *.ps *.svg *.eventlog

repl:
	cabal repl lib:helma

report:
	make haddock stan hlint
	./report.sh

run:
	make tix
	cabal run --jobs=$(JOBS) helma

sdist:
	cabal sdist

stan:
	./stan.sh
	mv stan.html docs/reports

stylish:
	stylish-haskell -r -v -i hs

test:
	cabal test --jobs=$(JOBS) --test-show-details=streaming -f ghcoptions

tix:
	rm -f helma.tix

update:
	cabal update
