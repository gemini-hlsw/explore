#!/bin/bash

# WARNING: This will apply CURRENT LOCAL migrations to the staging server,
# which may not necessarily be in sync with the dev server.
cd hasura/user-prefs
unset NODE_OPTIONS
hasura migrate apply --endpoint https://user-prefs-staging.herokuapp.com --database-name default
hasura metadata apply --endpoint https://user-prefs-staging.herokuapp.com
cd ../..
firebase hosting:clone explore-gemini-dev:live explore-gemini-stage:live
