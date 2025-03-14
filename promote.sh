#!/bin/bash

# Usage: promote.sh <source_env> <target_env>
# Example: promote.sh dev staging

set -e

SOURCE_ENV=$1
TARGET_ENV=$2

if [ -z "$SOURCE_ENV" ] || [ -z "$TARGET_ENV" ]; then
  echo "Usage: promote.sh <source_env> <target_env>"
  exit 1
fi

echo "##### Capturing a db backup on $TARGET_ENV."
heroku pg:backups:capture --app lucuma-postgres-odb-${TARGET_ENV}

echo "##### Promoting all services from $SOURCE_ENV to $TARGET_ENV"

# Promote SSO
echo "Promote SSO to $TARGET_ENV"
heroku pipelines:promote -a lucuma-sso-${SOURCE_ENV} -t lucuma-sso-${TARGET_ENV}

# Promote ITC
echo "Promote ITC to $TARGET_ENV"
heroku pipelines:promote -a itc-${SOURCE_ENV} -t itc-${TARGET_ENV}

# Promote ODB
echo "Promote ODB to $TARGET_ENV"
heroku pipelines:promote -a lucuma-postgres-odb-${SOURCE_ENV} -t lucuma-postgres-odb-${TARGET_ENV}

# Update user preferences
echo "Promote user preferences db to $TARGET_ENV"
cd hasura/user-prefs
unset NODE_OPTIONS
HASURA_ENDPOINT="https://gpp-prefs-${TARGET_ENV}.lucuma.xyz"
hasura migrate apply --endpoint "$HASURA_ENDPOINT" --database-name default
hasura metadata apply --endpoint "$HASURA_ENDPOINT"
hasura metadata reload --endpoint "$HASURA_ENDPOINT"
