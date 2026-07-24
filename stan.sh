#!/usr/bin/env bash

export STAN_USE_DEFAULT_CONFIG=False
stan  --config-file=".stan.toml" -s --hide-solution report
