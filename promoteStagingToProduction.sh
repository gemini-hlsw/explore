#!/bin/bash

# WARNING: This will apply CURRENT LOCAL migrations to the production server,
# which may not necessarily be in sync with the staging server.
cd hasura/user-prefs
unset NODE_OPTIONS
hasura migrate apply --endpoint https://prefs.gpp.gemini.edu --database-name default
hasura metadata apply --endpoint https://prefs.gpp.gemini.edu
hasura metadata reload --endpoint https://prefs.gpp.gemini.edu
cd ../..
firebase hosting:clone explore-gemini-stage:live explore-gemini-prod:live
