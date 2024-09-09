#!/bin/bash

# WARNING: This will apply CURRENT LOCAL migrations to the staging server,
# which may not necessarily be in sync with the dev server.
cd hasura/user-prefs
hasura migrate apply --endpoint https://user-prefs-staging.herokuapp.com 
cd ../..
firebase hosting:clone explore-gemini-dev:live explore-gemini-stage:live