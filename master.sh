#!/bin/bash

mkdir -p ../helma/hs
rsync -av \
  hs/ ../helma/hs/

mkdir -p ../helma/examples
rsync -av \
  examples/ ../helma/examples/

rsync -av \
  helma.cabal ../helma/helma.cabal
