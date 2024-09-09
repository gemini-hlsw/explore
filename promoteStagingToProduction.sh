#!/bin/bash

# WARNING: This will apply CURRENT LOCAL migrations to the production server,
# which may not necessarily be in sync with the staging server.
cd hasura/user-prefs
hasura migrate apply --endpoint https://user-prefs.herokuapp.com 
cd ../..
firebase hosting:clone explore-gemini-stage:live explore-gemini-prod:live