#!/usr/bin/env bash

# Usage: promote.sh <source_env> <target_env> [--dry-run] [--skip-slack]
# Example: promote.sh dev staging
# Example: promote.sh dev staging --dry-run
# Example: promote.sh dev staging --skip-slack

set -e

SOURCE_ENV=$1
TARGET_ENV=$2
DRY_RUN=false
SKIP_SLACK=false

# Parse optional arguments
for arg in "$@"; do
  case $arg in
    --dry-run)
      DRY_RUN=true
      ;;
    --skip-slack)
      SKIP_SLACK=true
      ;;
  esac
done

if [ -z "$SOURCE_ENV" ] || [ -z "$TARGET_ENV" ]; then
  echo "Usage: promote.sh <source_env> <target_env> [--dry-run] [--skip-slack]"
  echo "Example: promote.sh dev staging"
  echo "Example: promote.sh dev staging --dry-run"
  echo "Example: promote.sh dev staging --skip-slack"
  echo "Example: promote.sh dev staging --dry-run --skip-slack"
  exit 1
fi

# Validate environment names against allowlist
VALID_ENVS=("dev" "staging" "production")
if ! [[ " ${VALID_ENVS[*]} " =~ " ${SOURCE_ENV} " ]] || ! [[ " ${VALID_ENVS[*]} " =~ " ${TARGET_ENV} " ]]; then
  echo "Error: Invalid environment names. Must be one of: ${VALID_ENVS[*]}"
  echo "Provided: SOURCE_ENV='$SOURCE_ENV', TARGET_ENV='$TARGET_ENV'"
  exit 1
fi

# Check environment variables
echo "##### Environment Variables Status"
echo "GPP_SLACK_WEBHOOK_URL: $([ -n "$GPP_SLACK_WEBHOOK_URL" ] && echo "✓ Set" || echo "✗ Not set")"
echo "GPP_SLACK_CHANNEL: $([ -n "$GPP_SLACK_CHANNEL" ] && echo "✓ Set ($GPP_SLACK_CHANNEL)" || echo "✗ Not set (will use default: #gpp)")"
echo "GPP_GITHUB_TOKEN: $([ -n "$GPP_GITHUB_TOKEN" ] && echo "✓ Set" || echo "✗ Not set (GitHub API may be limited)")"
echo

# Global variables to track what needs promotion
PROMOTE_SSO=false
PROMOTE_ITC=false
PROMOTE_ODB=false
PROMOTE_HASURA=false
PROMOTE_FIREBASE=false

# Helper function to map environment names for GitHub deployments
map_github_deploy_env() {
  local env=$1
  case $env in
    "dev") echo "development" ;;
    "staging") echo "staging" ;;
    "production") echo "production" ;;
    *) echo "explore-${env}" ;;
  esac
}

# Slack notification functions

get_git_compare_link() {
  service=$1
  source_env=$2
  target_env=$3
  pre_promotion_target_sha=${4:-""}

  # Get service-specific repository and commit SHAs
  github_repo=""
  source_sha=""
  target_sha=""

  case $service in
    "SSO")
      github_repo="gemini-hlsw/lucuma-sso"
      # Get commit SHAs from Heroku
      source_sha=$(heroku releases:info --json --app "lucuma-sso-${source_env}" 2>/dev/null | jq -r '.description' | sed -n 's/Deploy \([a-f0-9]\{7,\}\).*/\1/p' || echo "unknown")
      target_sha=$(heroku releases:info --json --app "lucuma-sso-${target_env}" 2>/dev/null | jq -r '.description' | sed -n 's/Deploy \([a-f0-9]\{7,\}\).*/\1/p' || echo "unknown")
      ;;
    "ITC")
      github_repo="gemini-hlsw/lucuma-itc"
      source_sha="unknown"
      target_sha="unknown"
      ;;
    "ODB")
      github_repo="gemini-hlsw/lucuma-odb"
      # Get commit SHAs from Heroku release
      source_sha=$(heroku releases:info --json --app "lucuma-postgres-odb-${source_env}" 2>/dev/null | jq -r '.description' | sed -n 's/Deploy \([a-f0-9]\{7,\}\).*/\1/p' || echo "unknown")

      if [ -n "$pre_promotion_target_sha" ] && [ "$pre_promotion_target_sha" != "unknown" ]; then
        target_sha="$pre_promotion_target_sha"
      else
        target_sha=$(heroku releases:info --json --app "lucuma-postgres-odb-${target_env}" 2>/dev/null | jq -r '.description' | sed -n 's/Deploy \([a-f0-9]\{7,\}\).*/\1/p' || echo "unknown")
      fi
      ;;
    "Explore")
      github_repo="gemini-hlsw/explore"
      # Get commit SHAs from GitHub deployments API
      source_deploy_env=$(map_github_deploy_env "$source_env")
      target_deploy_env=$(map_github_deploy_env "$target_env")

      # Get the most recent deployments with secure auth handling
      local curl_opts=("-s" "-H" "Accept: application/vnd.github.v3+json")
      if [ -n "$GPP_GITHUB_TOKEN" ]; then
        curl_opts+=("-H" "Authorization: Bearer $GPP_GITHUB_TOKEN")
      fi

      source_sha=$(curl "${curl_opts[@]}" \
        "https://api.github.com/repos/gemini-hlsw/explore/deployments?environment=${source_deploy_env}&per_page=1" \
        | jq -r '.[0].sha // "unknown"' 2>/dev/null || echo "unknown")
      target_sha=$(curl "${curl_opts[@]}" \
        "https://api.github.com/repos/gemini-hlsw/explore/deployments?environment=${target_deploy_env}&per_page=1" \
        | jq -r '.[0].sha // "unknown"' 2>/dev/null || echo "unknown")
      ;;
    *)
      github_repo="gemini-hlsw/lucuma-odb"
      source_sha="unknown"
      target_sha="unknown"
      ;;
  esac

  # Create appropriate GitHub link based on available commit info
  if [ "$source_sha" != "unknown" ] && [ "$target_sha" != "unknown" ]; then
    if [ "$source_sha" != "$target_sha" ]; then
      echo "https://github.com/$github_repo/compare/${target_sha}...${source_sha}"
    else
      echo ""  # Skip meaningless comparison when SHAs are identical
    fi
  elif [ "$source_sha" != "unknown" ]; then
    echo "https://github.com/$github_repo/commit/${source_sha}"
  else
    echo ""  # Skip link when no commit info available
  fi
}

show_slack_message() {
  local service=$1
  local source_env=$2
  local target_env=$3
  local backup_id=${4:-""}
  local pre_promotion_target_sha=${5:-""}
  local compare_link=$(get_git_compare_link "$service" "$source_env" "$target_env" "$pre_promotion_target_sha")
  local channel=${GPP_SLACK_CHANNEL:-"#gpp"}
  local timestamp=$(date -u "+%Y-%m-%d %H:%M:%S UTC")

  echo "  Slack notification to $channel:"
  echo "    $service deployed from $source_env → $target_env"

  if [ "$service" = "SSO" ] || [ "$service" = "ODB" ] || [ "$service" = "Explore" ]; then
    if [ -n "$compare_link" ]; then
      echo "    Changes: $compare_link"
    fi
  fi

  # Include backup ID for ODB
  if [ "$service" = "ODB" ] && [ -n "$backup_id" ] && [ "$backup_id" != "unknown" ]; then
    echo "    Database backup: $backup_id"
  fi

  echo "    Time: $timestamp"
  echo
}

send_slack_notification() {
  local service=$1
  local source_env=$2
  local target_env=$3
  local backup_id=${4:-""}
  local pre_promotion_target_sha=${5:-""}
  local compare_link=$(get_git_compare_link "$service" "$source_env" "$target_env" "$pre_promotion_target_sha")
  local timestamp=$(date -u "+%Y-%m-%d %H:%M:%S UTC")

  if [ "$SKIP_SLACK" = true ]; then
    echo "  ! Slack notifications disabled with --skip-slack flag"
    show_slack_message "$service" "$source_env" "$target_env" "$backup_id" "$pre_promotion_target_sha"
    return 0
  fi

  if [ -z "$GPP_SLACK_WEBHOOK_URL" ]; then
    echo "  ! GPP_SLACK_WEBHOOK_URL not set - skipping Slack notification"
    return 0
  fi

  # Basic URL validation for Slack webhook
  if [[ ! "$GPP_SLACK_WEBHOOK_URL" =~ ^https://hooks\.slack\.com/services/ ]]; then
    echo "  ! Invalid Slack webhook URL format - skipping notification"
    return 0
  fi

  local message="✅ $service deployed from $source_env → $target_env\nTime: $timestamp"

  # Include changes link for SSO, ODB and Explore
  if [ "$service" = "SSO" ] || [ "$service" = "ODB" ] || [ "$service" = "Explore" ]; then
    if [ -n "$compare_link" ]; then
      message="✅ $service deployed from $source_env → $target_env\nChanges: $compare_link"
    else
      message="✅ $service deployed from $source_env → $target_env"
    fi

    # Include backup ID for ODB only
    if [ "$service" = "ODB" ] && [ -n "$backup_id" ] && [ "$backup_id" != "unknown" ]; then
      message="$message\nDatabase backup: $backup_id"
    fi

    message="$message\nTime: $timestamp"
  fi

  local payload="{\"text\":\"$message\"}"

  if curl -s -X POST -H 'Content-type: application/json' --data "$payload" "$GPP_SLACK_WEBHOOK_URL" > /dev/null 2>&1; then
    echo "  Slack notification sent successfully"
  else
    echo "  ! Failed to send Slack notification (deployment continues)"
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

check_firebase_changes() {
  local source_env=$1
  local target_env=$2

  echo "Checking for Firebase hosting changes between $source_env and $target_env..."

  # Map environment names to GitHub deployment environment names
  local source_deploy_env=$(map_github_deploy_env "$source_env")
  local target_deploy_env=$(map_github_deploy_env "$target_env")

  # Get the most recent deployment SHAs for each environment from GitHub deployments API
  echo "  Querying deployments for ${source_deploy_env} and ${target_deploy_env}..."

  # Secure curl options handling
  local curl_opts=("-s" "-H" "Accept: application/vnd.github.v3+json")
  if [ -n "$GPP_GITHUB_TOKEN" ]; then
    curl_opts+=("-H" "Authorization: Bearer $GPP_GITHUB_TOKEN")
  fi

  local source_response=$(curl "${curl_opts[@]}" \
    "https://api.github.com/repos/gemini-hlsw/explore/deployments?environment=${source_deploy_env}&per_page=1" 2>/dev/null || echo "[]")
  local target_response=$(curl "${curl_opts[@]}" \
    "https://api.github.com/repos/gemini-hlsw/explore/deployments?environment=${target_deploy_env}&per_page=1" 2>/dev/null || echo "[]")

  local source_sha=$(echo "$source_response" | jq -r '.[0].sha // "none"' 2>/dev/null || echo "none")
  local target_sha=$(echo "$target_response" | jq -r '.[0].sha // "none"' 2>/dev/null || echo "none")

  # Debug output
  if [ "$source_sha" = "none" ]; then
    echo "  Debug: No deployment found for ${source_deploy_env}"
    echo "  API response: $(echo "$source_response" | head -c 200)..."
  fi
  if [ "$target_sha" = "none" ]; then
    echo "  Debug: No deployment found for ${target_deploy_env}"
    echo "  API response: $(echo "$target_response" | head -c 200)..."
  fi

  if [ "$source_sha" = "none" ] || [ "$target_sha" = "none" ]; then
    echo "  ! Could not retrieve deployment information - assuming changes exist for safety"
    return 0
  fi

  if [ "$source_sha" != "$target_sha" ]; then
    echo "  ✓ Firebase hosting has changes (source: $source_sha, target: $target_sha)"
    return 0  # Changes detected
  else
    echo "  ✗ Firebase hosting has no changes (both at $source_sha)"
    return 1  # No changes
  fi
}

## TODO sync hasura istanecs, we always get that changes are needed
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

  local has_changes=false

  # Check for pending migrations
  local pending_migrations=$(hasura migrate status --endpoint "$HASURA_ENDPOINT" --database-name default 2>/dev/null | grep "Not Present" | wc -l || echo "0")

  if [ "$pending_migrations" -gt 0 ]; then
    echo "  ✓ Hasura has $pending_migrations pending migration(s)"
    has_changes=true
  else
    echo "  ✗ Hasura has no pending migrations"
  fi

  # Check for metadata differences
  echo "  Checking metadata differences..."
  local metadata_diff_output=$(hasura metadata diff --endpoint "$HASURA_ENDPOINT" 2>/dev/null || echo "error")

  if [ "$metadata_diff_output" = "error" ]; then
    echo "  ! Could not check metadata diff - assuming changes needed"
    has_changes=true
  elif [ -n "$metadata_diff_output" ] && [ "$metadata_diff_output" != "Metadata is consistent" ]; then
    echo "  ✓ Hasura metadata has differences:"
    echo "$metadata_diff_output" | head -5 | sed 's/^/    /'
    if [ $(echo "$metadata_diff_output" | wc -l) -gt 5 ]; then
      echo "    ... (truncated, $(echo "$metadata_diff_output" | wc -l) total lines)"
    fi
    has_changes=true
  else
    echo "  ✗ Hasura metadata is consistent"
  fi

  cd - > /dev/null

  if [ "$has_changes" = true ]; then
    return 0  # Changes detected
  else
    echo "  ✗ No Hasura changes detected (migrations and metadata are up to date)"
    return 1  # No changes
  fi
}

# Function to promote Docker images between environments in Heroku
# Uses associative array for image names and separate arrays for process types
# Example usage:
#   image_name_ITC="lucuma-itc"
#   process_types_ITC=("web")
#   backup_ITC=false
#   image_name_ODB="lucuma-postgres-odb"
#   process_types_ODB=("web" "calibration" "obscalc")
#   backup_ODB=true
#   promote_heroku_docker_images ("ITC" "ODB")
promote_heroku_docker_images() {
  local -a systems=("${@:1}")
  
  # Login to Heroku container registry once (for all images)
  if ! heroku container:login; then
    echo "  ! Failed to login to Heroku container registry"
    exit 1
  fi
  
  # Process each entry in the array
  for display_name in "${systems[@]}"; do
    local promote_var="PROMOTE_${display_name}"
    
    # Check if this service should be promoted
    if [ "${!promote_var}" = true ]; then
      echo "## Promote ${display_name} to $TARGET_ENV"
      
      # Get image name from the associative array
      local image_name_var="image_name_${display_name}"
      local image_name="${!image_name_var}"

      echo "Capturing ${display_name} target environment state before promotion..."
      local TARGET_SHA_BEFORE=$(heroku releases:info --json --app "${image_name}-${TARGET_ENV}" 2>/dev/null | jq -r '.description' | sed -n 's/Deployed \(.*\)/\1/p' || echo "unknown")
      echo "${display_name} target environment SHA before promotion: ${TARGET_SHA_BEFORE}"

      local do_backup_var="backup_${display_name}"
      local backup_id="omited"
      if [ "${!do_backup_var}" = true ]; then
        echo "Capturing database backup before ${display_name} promotion..."
        local backup_output=$(heroku pg:backups:capture --app ${image_name}-${TARGET_ENV} 2>&1)
        local backup_id=$(echo "$backup_output" | grep -o 'b[0-9]\{3\}' | head -1 || echo "unknown")
        if [ "$backup_id" != "unknown" ]; then
          echo "${display_name} database backup created: ${backup_id}"
        else
          echo "${display_name} database backup created (ID not captured)"
        fi
      fi
      
      # Get process types from the corresponding array variable
      # We use indirect reference with the display_name to access the array
      local process_types_var="process_types_${display_name}[@]"
      local process_types=("${!process_types_var}")
      
      # Pull, tag, and push all process types for this image
      for process_type in "${process_types[@]}"; do
        echo "  Processing ${image_name}/${process_type}"
        
        # Pull the source image
        if ! docker pull "registry.heroku.com/${image_name}-${SOURCE_ENV}/${process_type}:latest"; then
          echo "  ! Failed to pull ${image_name}-${SOURCE_ENV}/${process_type}:latest"
          exit 1
        fi
        
        # Tag with target environment
        if ! docker tag "registry.heroku.com/${image_name}-${SOURCE_ENV}/${process_type}:latest" "registry.heroku.com/${image_name}-${TARGET_ENV}/${process_type}:latest"; then
          echo "  ! Failed to tag ${image_name}-${TARGET_ENV}/${process_type}:latest"
          exit 1
        fi
        
        # Push to target environment
        if ! docker push "registry.heroku.com/${image_name}-${TARGET_ENV}/${process_type}:latest"; then
          echo "  ! Failed to push ${image_name}-${TARGET_ENV}/${process_type}:latest"
          exit 1
        fi
      done
      
      # Release all process types for this image in a single command
      echo "  Releasing all process types for ${image_name}-${TARGET_ENV}..."
      if heroku container:release "${process_types[@]}" --app "${image_name}-${TARGET_ENV}"; then
        echo "  ✓ ${display_name} promotion successful"
        send_slack_notification "${display_name}" "$SOURCE_ENV" "$TARGET_ENV" "$backup_id" "$TARGET_SHA_BEFORE"
      else
        echo "  ! ${display_name} promotion FAILED at release stage"
        exit 1
      fi
      
      echo
    else
      echo "## Skipping ${display_name} promotion (no changes)"
      echo
    fi
  done
}

echo "##### Checking for changes between $SOURCE_ENV and $TARGET_ENV"
echo

if check_docker_changes "SSO" "lucuma-sso-${SOURCE_ENV}" "lucuma-sso-${TARGET_ENV}"; then
  PROMOTE_SSO=true
fi

if check_docker_changes "ITC" "itc-${SOURCE_ENV}" "itc-${TARGET_ENV}"; then
  PROMOTE_ITC=true
fi

if check_docker_changes "ODB" "lucuma-postgres-odb-${SOURCE_ENV}" "lucuma-postgres-odb-${TARGET_ENV}"; then
  PROMOTE_ODB=true
fi

if check_hasura_changes; then
  PROMOTE_HASURA=true
fi

case $SOURCE_ENV in
  "dev")
    if check_firebase_changes "dev" "staging"; then
      PROMOTE_FIREBASE=true
    fi
    ;;
  "staging")
    if check_firebase_changes "staging" "production"; then
      PROMOTE_FIREBASE=true
    fi
    ;;
  *)
    echo "Unknown source environment: $SOURCE_ENV - skipping Firebase check"
    ;;
esac

echo

if [ "$PROMOTE_SSO" = false ] && [ "$PROMOTE_ITC" = false ] && [ "$PROMOTE_ODB" = false ] && [ "$PROMOTE_HASURA" = false ] && [ "$PROMOTE_FIREBASE" = false ]; then
  echo "##### No changes detected - nothing to promote!"
  echo "All services are already up to date between $SOURCE_ENV and $TARGET_ENV"
  exit 0
fi

# Show what will be promoted
echo "##### Summary of services to promote:"
[ "$PROMOTE_SSO" = true ] && echo "  • SSO will be promoted"
[ "$PROMOTE_ITC" = true ] && echo "  • ITC will be promoted"
[ "$PROMOTE_ODB" = true ] && echo "  • ODB will be promoted"
[ "$PROMOTE_HASURA" = true ] && echo "  • Hasura migrations will be applied"
[ "$PROMOTE_FIREBASE" = true ] && echo "  • Firebase hosting will be promoted"
echo

if [ "$DRY_RUN" = true ]; then
  echo "##### DRY RUN MODE - No actual promotions will be performed"
  echo
  echo "##### Slack notifications that would be sent:"
  [ "$PROMOTE_SSO" = true ] && show_slack_message "SSO" "$SOURCE_ENV" "$TARGET_ENV"
  [ "$PROMOTE_ITC" = true ] && show_slack_message "ITC" "$SOURCE_ENV" "$TARGET_ENV"
  [ "$PROMOTE_ODB" = true ] && show_slack_message "ODB" "$SOURCE_ENV" "$TARGET_ENV" "b###"
  [ "$PROMOTE_FIREBASE" = true ] && show_slack_message "Explore" "$SOURCE_ENV" "$TARGET_ENV"
  exit 0
fi

echo "##### Promoting services from $SOURCE_ENV to $TARGET_ENV"
if [ "$SKIP_SLACK" = true ]; then
  echo "Slack notifications: DISABLED (--skip-slack flag)"
else
  echo "Slack notifications: ENABLED"
fi
echo

image_name_SSO="lucuma-sso"
process_types_SSO=("web")
backup_SSO=true

image_name_ITC="itc"
process_types_ITC=("web")
backup_ITC=false

image_name_ODB="lucuma-postgres-odb"
process_types_ODB=("web" "calibration" "obscalc")
backup_ODB=true

systems=("SSO" "ITC" "ODB")

promote_heroku_docker_images "${systems[@]}"

# Update user preferences
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
  (
    cd hasura/user-prefs || exit 1
    unset NODE_OPTIONS
    if hasura migrate apply --endpoint "$HASURA_ENDPOINT" --database-name default && \
      hasura metadata apply --endpoint "$HASURA_ENDPOINT" && \
      hasura metadata reload --endpoint "$HASURA_ENDPOINT"; then
      echo "  ✓ Hasura migration successful"
    else
      echo "  ! Hasura migration FAILED"
      exit 1
    fi
  )
  if [ $? -ne 0 ]; then
    echo "  ! Hasura promotion failed"
    exit 1
  fi
  echo
else
  echo "## Skipping Hasura migration (no changes)"
  echo
fi

# Promote Firebase hosting
if [ "$PROMOTE_FIREBASE" = true ]; then
  case $SOURCE_ENV in
    "dev")
      echo "## Promote Firebase hosting to staging"
      if firebase hosting:clone explore-gemini-dev:live explore-gemini-stage:live; then
        echo "  ✓ Firebase hosting promoted successfully"
      else
        echo "  ! Firebase hosting promotion FAILED"
        exit 1
      fi

      # Record deployment to GitHub deployments API
      echo "  Recording deployment to GitHub..."

      # Get the SHA from the source environment (dev) that's actually deployed
      echo "  Debug: Querying development environment for deployed SHA..."
      if [ -n "$GPP_GITHUB_TOKEN" ]; then
        echo "  Debug: curl -s -H \"Accept: application/vnd.github.v3+json\" -H \"Authorization: Bearer \$GPP_GITHUB_TOKEN\" \"https://api.github.com/repos/gemini-hlsw/explore/deployments?environment=development&per_page=1\""
        deployment_response=$(curl -s -H "Accept: application/vnd.github.v3+json" \
          -H "Authorization: Bearer $GPP_GITHUB_TOKEN" \
          "https://api.github.com/repos/gemini-hlsw/explore/deployments?environment=development&per_page=1" 2>&1)
        echo "  Debug: GitHub API response: $(echo "$deployment_response" | head -c 200)..."
        source_sha=$(echo "$deployment_response" | jq -r '.[0].sha // "unknown"' 2>/dev/null || echo "unknown")
      else
        source_sha="unknown"
      fi
      echo "  Debug: Development environment SHA: $source_sha"

      if [ "$source_sha" != "unknown" ] && [ -n "$GPP_GITHUB_TOKEN" ]; then
        echo "  Debug: Creating deployment record for staging environment..."
        echo "  Debug: curl -s -X POST \"https://api.github.com/repos/gemini-hlsw/explore/deployments\" -H \"Accept: application/vnd.github.v3+json\" -H \"Authorization: Bearer \$GPP_GITHUB_TOKEN\" -d '{\"ref\":\"\$source_sha\",\"environment\":\"staging\",\"description\":\"Firebase hosting promotion from dev to staging\"}'"
        deployment_result=$(curl -s -X POST "https://api.github.com/repos/gemini-hlsw/explore/deployments" \
          -H "Accept: application/vnd.github.v3+json" \
          -H "Authorization: Bearer $GPP_GITHUB_TOKEN" \
          -d "{\"ref\":\"$source_sha\",\"environment\":\"staging\",\"description\":\"Firebase hosting promotion from dev to staging\"}" \
          2>&1)
        echo "  Debug: Deployment creation result: $(echo "$deployment_result" | head -c 200)..."

        if echo "$deployment_result" | grep -q '"id"'; then
          echo "  ✓ Deployment record created successfully"
        else
          echo "  ! Failed to record deployment: $deployment_result"
        fi
      elif [ "$source_sha" = "unknown" ]; then
        echo "  ! Could not determine deployed SHA from development environment"
      else
        echo "  ! No GitHub token - skipping deployment record creation"
      fi

      send_slack_notification "Explore" "$SOURCE_ENV" "$TARGET_ENV"
      ;;
    "staging")
      echo "## Promote Firebase hosting to production"
      if firebase hosting:clone explore-gemini-stage:live explore-gemini-prod:live; then
        echo "  ✓ Firebase hosting promoted successfully"
      else
        echo "  ! Firebase hosting promotion FAILED"
        exit 1
      fi

      # Record deployment to GitHub deployments API
      echo "  Recording deployment to GitHub..."

      # Get the SHA from the source environment (staging) that's actually deployed
      echo "  Debug: Querying staging environment for deployed SHA..."
      if [ -n "$GPP_GITHUB_TOKEN" ]; then
        echo "  Debug: curl -s -H \"Accept: application/vnd.github.v3+json\" -H \"Authorization: Bearer \$GPP_GITHUB_TOKEN\" \"https://api.github.com/repos/gemini-hlsw/explore/deployments?environment=staging&per_page=1\""
        deployment_response=$(curl -s -H "Accept: application/vnd.github.v3+json" \
          -H "Authorization: Bearer $GPP_GITHUB_TOKEN" \
          "https://api.github.com/repos/gemini-hlsw/explore/deployments?environment=staging&per_page=1" 2>&1)
        echo "  Debug: GitHub API response: $(echo "$deployment_response" | head -c 200)..."
        source_sha=$(echo "$deployment_response" | jq -r '.[0].sha // "unknown"' 2>/dev/null || echo "unknown")
      else
        source_sha="unknown"
      fi
      echo "  Debug: Staging environment SHA: $source_sha"

      if [ "$source_sha" != "unknown" ] && [ -n "$GPP_GITHUB_TOKEN" ]; then
        echo "  Debug: Creating deployment record for production environment..."
        echo "  Debug: curl -s -X POST \"https://api.github.com/repos/gemini-hlsw/explore/deployments\" -H \"Accept: application/vnd.github.v3+json\" -H \"Authorization: Bearer \$GPP_GITHUB_TOKEN\" -d '{\"ref\":\"\$source_sha\",\"environment\":\"production\",\"description\":\"Firebase hosting promotion from staging to production\"}'"
        deployment_result=$(curl -s -X POST "https://api.github.com/repos/gemini-hlsw/explore/deployments" \
          -H "Accept: application/vnd.github.v3+json" \
          -H "Authorization: Bearer $GPP_GITHUB_TOKEN" \
          -d "{\"ref\":\"$source_sha\",\"environment\":\"production\",\"description\":\"Firebase hosting promotion from staging to production\"}" \
          2>&1)
        echo "  Debug: Deployment creation result: $(echo "$deployment_result" | head -c 200)..."

        if echo "$deployment_result" | grep -q '"id"'; then
          echo "  ✓ Deployment record created successfully"
        else
          echo "  ! Failed to record deployment: $deployment_result"
        fi
      elif [ "$source_sha" = "unknown" ]; then
        echo "  ! Could not determine deployed SHA from staging environment"
      else
        echo "  ! No GitHub token - skipping deployment record creation"
      fi

      send_slack_notification "Explore" "$SOURCE_ENV" "$TARGET_ENV"
      ;;
    *)
      echo "## Unknown source environment for Firebase: $SOURCE_ENV"
      ;;
  esac
  echo
else
  echo "## Skipping Firebase hosting promotion (no changes)"
  echo
fi

# Final summary
echo "##### Promotion completed!"
echo "Services promoted:"
[ "$PROMOTE_SSO" = true ] && echo "  ✓ SSO"
[ "$PROMOTE_ITC" = true ] && echo "  ✓ ITC"
[ "$PROMOTE_ODB" = true ] && echo "  ✓ ODB"
[ "$PROMOTE_HASURA" = true ] && echo "  ✓ Hasura migrations"
[ "$PROMOTE_FIREBASE" = true ] && echo "  ✓ Firebase hosting"
echo "Services skipped (no changes):"
[ "$PROMOTE_SSO" = false ] && echo "  ✗ SSO"
[ "$PROMOTE_ITC" = false ] && echo "  ✗ ITC"
[ "$PROMOTE_ODB" = false ] && echo "  ✗ ODB"
[ "$PROMOTE_HASURA" = false ] && echo "  ✗ Hasura migrations"
[ "$PROMOTE_FIREBASE" = false ] && echo "  ✗ Firebase hosting"
