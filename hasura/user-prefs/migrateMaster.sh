#! /bin/bash

# WARNING: This will apply CURRENT LOCAL migrations
 unset NODE_OPTIONS
 hasura migrate apply --endpoint https://user-prefs-master.herokuapp.com --database-name default
 hasura metadata apply --endpoint https://user-prefs-master.herokuapp.com
 hasura metadata reload --endpoint https://user-prefs-master.herokuapp.com
