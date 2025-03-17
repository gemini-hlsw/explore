#!/bin/bash

pushd .
source ./promote.sh staging production

popd
firebase hosting:clone explore-gemini-stage:live explore-gemini-prod:live
