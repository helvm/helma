.PHONY: all bench build check clean configure exec fast golden haddock hlint install main output repl report run sdist stan stylish test tix update

all: update fast install sdist bench

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
	if test -d .hie; then rm -r .hie; fi

configure:
	rm -f cabal.project.local*
	cabal configure --enable-benchmarks --enable-tests -f ghcoptions

exec:
	make tix
	cabal new-exec --jobs helma

fast: main report

golden:
	if test -d .output/golden; then rm -r .output/golden; fi

haddock:
	cabal new-haddock

hlint:
	./hlint.sh

install:
	cabal install all --overwrite-policy=always

main:
	make stylish configure check build test

output:
	if test -d .output; then rm -r .output; fi

repl:
	cabal new-repl lib:helma

report:
	make haddock stan hlint
	./report.sh

run:
	make tix
	cabal new-run --jobs helma

sdist:
	cabal sdist

stan:
	./stan.sh

stylish:
	stylish-haskell -r -v -i hs

test:
	rm -f hspec-discover.tix
	cabal new-test --jobs --test-show-details=streaming -f ghcoptions

tix:
	rm -f helma.tix

update:
	cabal update
