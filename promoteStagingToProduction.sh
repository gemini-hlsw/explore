#!/bin/bash

# WARNING: This will apply CURRENT LOCAL migrations to the production server,
# which may not necessarily be in sync with the staging server.
cd hasura/user-prefs
unset NODE_OPTIONS
hasura migrate apply --endpoint https://user-prefs.herokuapp.com --database-name default
hasura metadata apply --endpoint https://user-prefs.herokuapp.com
hasura metadata reload --endpoint https://user-prefs.herokuapp.com
cd ../..
firebase hosting:clone explore-gemini-stage:live explore-gemini-prod:live
