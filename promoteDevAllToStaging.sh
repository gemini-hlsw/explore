#!/bin/bash

DRY_RUN=${1:-}

pushd .
source ./promote.sh dev staging $DRY_RUN
popd
