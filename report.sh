#!/usr/bin/env bash

mkdir_and_cp() {
  mkdir -p $(dirname "$2") && cp -r "$1" "$2"
}

mkdir_and_cp dist-newstyle/build/*/*/*/doc/html/helma/ docs/reports
mkdir_and_cp dist-newstyle/build/*/*/*/hpc/vanilla/html/helma-test/ docs/reports
