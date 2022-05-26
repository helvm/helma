#!/usr/bin/env bash

#curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s .
hlint . --report=hlint.html --timing
mv hlint.html docs/reports
