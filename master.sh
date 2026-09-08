#!/bin/bash

mkdir -p ../helma/hs
rsync -av \
  hs/ ../helma/hs/

mkdir -p ../helma/examples
rsync -av \
  examples/ ../helma/examples/

mkdir -p ../helma/.output
rsync -av \
  .output/ ../helma/.output/

rsync -av \
  helma.cabal ../helma/helma.cabal
