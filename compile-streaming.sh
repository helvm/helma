#!/usr/bin/env bash

rm -rf cabal.project.local
rm -rf .hie

cabal new-clean &&
cabal new-build &&
cabal configure --enable-tests --enable-coverage &&
cabal new-test --test-show-details=streaming &&
./stan.sh

cp stan.html docs/reports
