#!/bin/bash

DRY_RUN=${1:-}

pushd .
source ./promote.sh staging production $DRY_RUN
popd
