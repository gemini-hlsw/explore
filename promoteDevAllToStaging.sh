#!/bin/bash

pushd .
source ./promote.sh dev staging

popd
echo "Promote explore to staging"
firebase hosting:clone explore-gemini-dev:live explore-gemini-stage:live
