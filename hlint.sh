#!/usr/bin/env bash

hlint . --report=hlint.html --timing

mv hlint.html docs/reports

curl -sSL https://raw.github.com/ndmitchell/hlint/master/misc/run.sh | sh -s .
