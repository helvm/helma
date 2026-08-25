#!/bin/bash

mkdir -p ../helma/hs

rsync -av \
  --exclude='src/HelVM/HelMA/Automata/Piet/LLVM/' \
  --exclude='test/HelVM/HelMA/Automata/Piet/LLVM/' \
  hs/ ../helma/hs/
