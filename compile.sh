#!/usr/bin/env bash

rm -rf cabal.project.local
rm -rf .hie

cabal new-clean &&
cabal new-build &&
configure --enable-tests --enable-coverage &&
cabal new-test &&
./stan.sh

cp stan.html docs/reports
