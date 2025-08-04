#!/bin/bash

DRY_RUN=${1:-}

# All promotion logic is now centralized in promote.sh
# This includes both backend services and Firebase hosting
pushd .
source ./promote.sh dev staging $DRY_RUN
popd