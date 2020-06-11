#!/usr/bin/env bash

./hlint.sh

mv .hlint.yaml .hlint.yaml.bak

./hlint.sh

mv .hlint.yaml.bak .hlint.yaml
