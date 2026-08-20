#!/bin/bash

mkdir -p ../helma/hs

rsync -av \
  --exclude='app/' \
  --exclude='src/HelVM/HelMA/Automata/Piet/Free/' \
  --exclude='test/HelVM/HelMA/Automata/Piet/Free/' \
  hs/ ../helma/hs/
