#!/bin/bash

# WARNING: This will apply CURRENT LOCAL migrations to the staging server,
# which may not necessarily be in sync with the dev server.
cd hasura/user-prefs
unset NODE_OPTIONS
hasura migrate apply --endpoint https://gpp-prefs-staging.lucuma.xyz --database-name default
hasura metadata apply --endpoint https://gpp-prefs-staging.lucuma.xyz
hasura metadata reload --endpoint https://gpp-prefs-staging.lucuma.xyz
cd ../..
firebase hosting:clone explore-gemini-dev:live explore-gemini-stage:live
