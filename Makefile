.PHONY: all bench build check check-whitespace clean configure exec fast golden haddock hlint hpack install main output profile-clean profile-cost profile-heap profile-test repl report run sdist stan stylish test tix update

# Ustaw bezpieczny limit wątków na podstawie dostępnego RAMu (np. 2 lub 4 zamiast bezlimitowego --jobs)
JOBS ?= 2

all: update fast

bench:
	rm -f helma-benchmark.tix
	cabal new-bench --jobs=$(JOBS) -f ghcoptions

build:
	cabal new-build --jobs=$(JOBS) -f ghcoptions

check:
	cabal check

check-whitespace:
	git check-whitespace

clean: profile-clean
	cabal new-clean
	if test -d .cabal-sandbox; then rm -rf .cabal-sandbox; fi
	if test -d .hpc; then rm -rf .hpc; fi
	if test -d .hie; then rm -rf .hie; fi

configure:
	rm -f cabal.project.local*
	cabal configure --enable-tests -f ghcoptions

exec:
	make tix
	cabal new-exec --jobs=$(JOBS) helma

fast: main report sdist install

golden:
	if test -d .output/golden; then rm -r .output/golden; fi

haddock:
	cabal new-haddock

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

profile-test: profile-clean
	cabal new-run --jobs=$(JOBS) -f ghcoptions helma-test --enable-profiling --ghc-options="-fprof-auto" -- +RTS -p -s

profile-heap: profile-clean
	cabal new-run --jobs=$(JOBS) -f ghcoptions helma-test --enable-profiling --ghc-options="-fprof-auto" -- +RTS -hy -l
	@if command -v hp2pretty >/dev/null 2>&1; then hp2pretty *.hp; elif command -v hp2ps >/dev/null 2>&1; then hp2ps -c *.hp; fi

profile-cost: profile-clean
	cabal new-run --jobs=$(JOBS) -f ghcoptions helma-test --enable-profiling --ghc-options="-fprof-auto" -- +RTS -hc -l
	@if command -v hp2pretty >/dev/null 2>&1; then hp2pretty *.hp; elif command -v hp2ps >/dev/null 2>&1; then hp2ps -c *.hp; fi

profile-clean:
	rm -f *.prof *.hp *.ps *.svg *.eventlog

repl:
	cabal new-repl lib:helma

report:
	make haddock stan hlint
	./report.sh

run:
	make tix
	cabal new-run --jobs=$(JOBS) helma

sdist:
	cabal sdist

stan:
	./stan.sh
	mv stan.html docs/reports

stylish:
	stylish-haskell -r -v -i hs

test:
	cabal new-test --jobs=$(JOBS) --test-show-details=streaming -f ghcoptions

tix:
	rm -f helma.tix

update:
	cabal update
