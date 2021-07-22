#!/usr/bin/env bash

export STAN_USE_DEFAULT_CONFIG=True

stan -s --hide-solution report

cp stan.html docs/reports
