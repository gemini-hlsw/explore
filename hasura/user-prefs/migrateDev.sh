#! /bin/bash

# WARNING: This will apply CURRENT LOCAL migrations
 unset NODE_OPTIONS
 hasura migrate apply --endpoint https://gpp-prefs-dev.lucuma.xyz --database-name default
 hasura metadata apply --endpoint https://gpp-prefs-dev.lucuma.xyz
 hasura metadata reload --endpoint https://gpp-prefs-dev.lucuma.xyz
