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
echo

echo "##### Promoting all services from $SOURCE_ENV to $TARGET_ENV"
echo

# Promote SSO
echo "## Promote SSO to $TARGET_ENV"
heroku pipelines:promote -a lucuma-sso-${SOURCE_ENV} -t lucuma-sso-${TARGET_ENV}
echo

# Promote ITC
echo "## Promote ITC to $TARGET_ENV"
heroku container:login
docker pull registry.heroku.com/itc-${SOURCE_ENV}/web:latest
docker tag registry.heroku.com/itc-${SOURCE_ENV}/web:latest registry.heroku.com/itc-${TARGET_ENV}/web:latest
docker push registry.heroku.com/itc-${TARGET_ENV}/web:latest
heroku container:release web --app itc-${TARGET_ENV}
echo

# Promote ODB
echo "## Promote ODB to $TARGET_ENV"
heroku pipelines:promote -a lucuma-postgres-odb-${SOURCE_ENV} -t lucuma-postgres-odb-${TARGET_ENV}
echo

# Update user preferences
HASURA_ENDPOINT=""
case $TARGET_ENV in
  "production")
    HASURA_ENDPOINT="https://prefs.gpp.gemini.edu"
    ;;
  "staging")
    HASURA_ENDPOINT="https://gpp-prefs-staging.lucuma.xyz"
    ;;
  *)
    echo "Unknown environment: $TARGET_ENV"
    exit 1
    ;;
esac

echo "## Promote user preferences db to $TARGET_ENV"
cd hasura/user-prefs
unset NODE_OPTIONS
hasura migrate apply --endpoint "$HASURA_ENDPOINT" --database-name default
hasura metadata apply --endpoint "$HASURA_ENDPOINT"
hasura metadata reload --endpoint "$HASURA_ENDPOINT"
