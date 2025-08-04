#!/bin/bash

# Usage: promote.sh <source_env> <target_env>
# Example: promote.sh dev staging

set -e

SOURCE_ENV=$1
TARGET_ENV=$2
DRY_RUN=${3:-false}

if [ -z "$SOURCE_ENV" ] || [ -z "$TARGET_ENV" ]; then
  echo "Usage: promote.sh <source_env> <target_env> [dry-run]"
  echo "Example: promote.sh dev staging"
  echo "Example: promote.sh dev staging dry-run"
  exit 1
fi

# Global variables to track what needs promotion
PROMOTE_SSO=false
PROMOTE_ITC=false
PROMOTE_ODB=false
PROMOTE_HASURA=false

# Change detection functions
check_pipeline_changes() {
  local service=$1
  local source_app=$2
  local target_app=$3

  echo "Checking for changes in $service..."

  # Get current release info for both environments
  local source_release=$(heroku releases:info --json --app "$source_app" | jq -r '.version')
  local target_release=$(heroku releases:info --json --app "$target_app" | jq -r '.version')
  local source_slug=$(heroku releases:info --json --app "$source_app" | jq -r '.slug.id // "none"')
  local target_slug=$(heroku releases:info --json --app "$target_app" | jq -r '.slug.id // "none"')

  if [ "$source_slug" != "$target_slug" ]; then
    echo "  ✓ $service has changes (source: $source_release/$source_slug, target: $target_release/$target_slug)"
    return 0  # Changes detected
  else
    echo "  ✗ $service has no changes (both at $target_release/$target_slug)"
    return 1  # No changes
  fi
}

check_docker_changes() {
  local service=$1
  local source_app=$2
  local target_app=$3

  echo "Checking for Docker image changes in $service..."

  # Get image digests
  local source_digest=$(heroku container:login > /dev/null 2>&1 && docker manifest inspect "registry.heroku.com/$source_app/web:latest" 2>/dev/null | jq -r '.config.digest // "none"' || echo "none")
  local target_digest=$(docker manifest inspect "registry.heroku.com/$target_app/web:latest" 2>/dev/null | jq -r '.config.digest // "none"' || echo "none")

  if [ "$source_digest" != "$target_digest" ]; then
    echo "  ✓ $service has changes (source: $source_digest, target: $target_digest)"
    return 0  # Changes detected
  else
    echo "  ✗ $service has no changes (both at $target_digest)"
    return 1  # No changes
  fi
}

check_hasura_changes() {
  echo "Checking for Hasura changes..."

  # Set up environment for Hasura endpoint
  local HASURA_ENDPOINT=""
  case $TARGET_ENV in
    "production")
      HASURA_ENDPOINT="https://prefs.gpp.gemini.edu"
      ;;
    "staging")
      HASURA_ENDPOINT="https://prefs-test.gpp.gemini.edu"
      ;;
    *)
      echo "  ! Unknown environment: $TARGET_ENV, assuming changes needed"
      return 0
      ;;
  esac

  cd hasura/user-prefs
  unset NODE_OPTIONS

  # Check for pending migrations
  local pending_migrations=$(hasura migrate status --endpoint "$HASURA_ENDPOINT" --database-name default 2>/dev/null | grep "Not Present" | wc -l || echo "0")

  if [ "$pending_migrations" -gt 0 ]; then
    echo "  ✓ Hasura has $pending_migrations pending migration(s)"
    cd - > /dev/null
    return 0  # Changes detected
  else
    echo "  ✗ Hasura has no pending migrations"
    cd - > /dev/null
    return 1  # No changes
  fi
}

echo "##### Checking for changes between $SOURCE_ENV and $TARGET_ENV"
echo

# Check each service for changes
if check_pipeline_changes "SSO" "lucuma-sso-${SOURCE_ENV}" "lucuma-sso-${TARGET_ENV}"; then
  PROMOTE_SSO=true
fi

if check_docker_changes "ITC" "itc-${SOURCE_ENV}" "itc-${TARGET_ENV}"; then
  PROMOTE_ITC=true
fi

if check_pipeline_changes "ODB" "lucuma-postgres-odb-${SOURCE_ENV}" "lucuma-postgres-odb-${TARGET_ENV}"; then
  PROMOTE_ODB=true
fi

if check_hasura_changes; then
  PROMOTE_HASURA=true
fi

echo

# Check if any promotions are needed
if [ "$PROMOTE_SSO" = false ] && [ "$PROMOTE_ITC" = false ] && [ "$PROMOTE_ODB" = false ] && [ "$PROMOTE_HASURA" = false ]; then
  echo "##### No changes detected - nothing to promote!"
  echo "All services are already up to date between $SOURCE_ENV and $TARGET_ENV"
  exit 0
fi

# Show what will be promoted
echo "##### Summary of changes detected:"
[ "$PROMOTE_SSO" = true ] && echo "  • SSO will be promoted"
[ "$PROMOTE_ITC" = true ] && echo "  • ITC will be promoted"
[ "$PROMOTE_ODB" = true ] && echo "  • ODB will be promoted"
[ "$PROMOTE_HASURA" = true ] && echo "  • Hasura migrations will be applied"
echo

if [ "$DRY_RUN" = "dry-run" ]; then
  echo "##### DRY RUN MODE - No actual promotions will be performed"
  exit 0
fi

echo "##### Capturing a db backup on $TARGET_ENV."
heroku pg:backups:capture --app lucuma-postgres-odb-${TARGET_ENV}
echo

echo "##### Promoting services from $SOURCE_ENV to $TARGET_ENV"
echo

# Promote SSO (conditional)
if [ "$PROMOTE_SSO" = true ]; then
  echo "## Promote SSO to $TARGET_ENV"
  heroku pipelines:promote -a lucuma-sso-${SOURCE_ENV} -t lucuma-sso-${TARGET_ENV}
  echo
else
  echo "## Skipping SSO promotion (no changes)"
  echo
fi

# Promote ITC (conditional)
if [ "$PROMOTE_ITC" = true ]; then
  echo "## Promote ITC to $TARGET_ENV"
  heroku container:login
  docker pull registry.heroku.com/itc-${SOURCE_ENV}/web:latest
  docker tag registry.heroku.com/itc-${SOURCE_ENV}/web:latest registry.heroku.com/itc-${TARGET_ENV}/web:latest
  docker push registry.heroku.com/itc-${TARGET_ENV}/web:latest
  heroku container:release web --app itc-${TARGET_ENV}
  echo
else
  echo "## Skipping ITC promotion (no changes)"
  echo
fi

# Promote ODB (conditional)
if [ "$PROMOTE_ODB" = true ]; then
  echo "## Promote ODB to $TARGET_ENV"
  heroku pipelines:promote -a lucuma-postgres-odb-${SOURCE_ENV} -t lucuma-postgres-odb-${TARGET_ENV}
  echo
else
  echo "## Skipping ODB promotion (no changes)"
  echo
fi

# Update user preferences (conditional)
if [ "$PROMOTE_HASURA" = true ]; then
  HASURA_ENDPOINT=""
  case $TARGET_ENV in
    "production")
      HASURA_ENDPOINT="https://prefs.gpp.gemini.edu"
      ;;
    "staging")
      HASURA_ENDPOINT="https://prefs-test.gpp.gemini.edu"
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
  cd - > /dev/null
  echo
else
  echo "## Skipping Hasura migration (no changes)"
  echo
fi

# Final summary
echo "##### Promotion completed!"
echo "Services promoted:"
[ "$PROMOTE_SSO" = true ] && echo "  ✓ SSO"
[ "$PROMOTE_ITC" = true ] && echo "  ✓ ITC"
[ "$PROMOTE_ODB" = true ] && echo "  ✓ ODB"
[ "$PROMOTE_HASURA" = true ] && echo "  ✓ Hasura migrations"
echo "Services skipped (no changes):"
[ "$PROMOTE_SSO" = false ] && echo "  ✗ SSO"
[ "$PROMOTE_ITC" = false ] && echo "  ✗ ITC"
[ "$PROMOTE_ODB" = false ] && echo "  ✗ ODB"
[ "$PROMOTE_HASURA" = false ] && echo "  ✗ Hasura migrations"
