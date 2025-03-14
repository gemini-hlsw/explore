#!/bin/bash

pushd .
source ./promote.sh staging production

popd
echo "firebase hosting:clone explore-gemini-stage:live explore-gemini-prod:live"
