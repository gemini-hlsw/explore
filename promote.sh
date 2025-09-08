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

all_systems=("Explore" "SSO" "ITC" "ODB")
docker_systems=("SSO" "ITC" "ODB")

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
if [ -n "$GPP_GITHUB_TOKEN" ]; then
    echo "GPP_GITHUB_TOKEN: ✓ Set"
  else 
    echo "GPP_GITHUB_TOKEN:  ✗ Not set (This is necessary for recording deployments)"
    exit 1
fi
echo

# Helper function to map environment names for GitHub deployments
map_github_deploy_env() {
  local env=$1
  case $env in
    "dev") echo "development" ;;
    "staging") echo "staging" ;;
    "production") echo "production" ;;
    *) echo "${env}" ;;
  esac
}

map_firebase_deploy_env() {
  local env=$1
    case $env in
    "dev") echo "dev" ;;
    "staging") echo "stage" ;;
    "production") echo "prod" ;;
    *) echo "${env}" ;;
  esac
}

# Slack notification functions
get_github_deployment_shas() {
  local system=$1
  local env=$2
  local -n git_sha="$3"

  local is_docker=$([[ " ${docker_systems[*]} " =~ [[:space:]]${system}[[:space:]] ]] && [[ ${4} ]] && echo true || echo false)
  
  if [ $is_docker = true ]; then
    local -n image_shas_object="$4"
  fi

  local repo_name=${repo["$system"]}
  local deploy_env=$(map_github_deploy_env "$env")

  # Secure curl options handling
  local curl_opts=("-s" "-H" "Accept: application/vnd.github.v3+json")
  if [ -n "$GPP_GITHUB_TOKEN" ]; then
    curl_opts+=("-H" "Authorization: Bearer $GPP_GITHUB_TOKEN")
  fi

  local github_deployment=$(curl "${curl_opts[@]}" \
    "https://api.github.com/repos/$repo_name/deployments?environment=${deploy_env}&task=deploy:${system}&per_page=1" || echo "error")

  if [ "$github_deployment" = "error" ]; then
    echo "  ! Error querying GitHub API Deployment for $system in $env"
    exit 1
  fi

  git_sha=$(echo "$github_deployment" | jq -r '.[0].sha // "none"' ) # 2>/dev/null)
  if [ $is_docker = true ]; then
    image_shas_object=$(echo "$github_deployment" | jq -r '.[0].payload.docker_image_shas // "none"' ) #2>/dev/null)
  fi
}

record_github_deployment() {
  local system=$1

  local repo_name=${repo["$system"]}
  local deploy_env=$(map_github_deploy_env "$TARGET_ENV")

  # Secure curl options handling
  local curl_opts=("-s" "-H" "Accept: application/vnd.github.v3+json" "-H" "Authorization: Bearer $GPP_GITHUB_TOKEN")
  local payload_object=""
  if [[ ${docker_image_shas_object["$system"]} ]]; then payload_object=", \"payload\": { \"docker_image_shas\": ${docker_image_shas_object["$system"]} }"; fi 


  echo "  Creating deployment record for $system in $TARGET_ENV environment..."
  # echo "  Debug: curl "${curl_opts[@]}" -X POST \"https://api.github.com/repos/$repo_name/deployments\" -d "{\"ref\":\"${source_sha["$system"]}\",\"environment\":\"$deploy_env\",\"description\":\"Promotion from $SOURCE_ENV to $TARGET_ENV\",\"required_contexts\": [],\"task\":\"deploy:$system\"$payload_object}""
  deployment_result=$(curl "${curl_opts[@]}" -X POST "https://api.github.com/repos/$repo_name/deployments" \
    -d "{\"ref\":\"${source_sha["$system"]}\",\"environment\":\"$deploy_env\",\"description\":\"Promotion from $SOURCE_ENV to $TARGET_ENV\",\"required_contexts\": [],\"task\":\"deploy:$system\"$payload_object}" \
    2>&1)
  # echo "  Debug: Deployment creation result: $(echo "$deployment_result" | head -c 200)..."

  if echo "$deployment_result" | grep -q '"id"'; then
    echo "  ✓ Deployment record created successfully"
  else
    echo "  ! Failed to record deployment: $deployment_result"
    exit 1
  fi
}

set_system_vars() {
  local -a systems=("${@:1}")

  for display_name in "${systems[@]}"; do

    echo "Checking of $display_name changes.."

    get_github_deployment_shas "$display_name" "$SOURCE_ENV" source_sha["$display_name"] docker_image_shas_object["$display_name"]

    if [[ "${source_sha["$display_name"]}" == "none" ]]; then
      echo "  ! Source SHA for $display_name in $SOURCE_ENV is not set - cannot proceed."
      exit 1
    fi

    if [[ " ${docker_systems[*]} " =~ [[:space:]]${display_name}[[:space:]] ]] && [[ "${docker_image_shas_object["$display_name"]}" == "none" ]]; then
      echo "  ! Docker image SHAs for $display_name in $SOURCE_ENV are not set - cannot proceed."
      exit 1
    fi

    get_github_deployment_shas "$display_name" "$TARGET_ENV" target_sha["$display_name"]
    
    promote["$display_name"]=$( [ "${source_sha["$display_name"]}" = "none" ] || [ "${target_sha["$display_name"]}" = "none" ] || [ "${source_sha["$display_name"]}" != "${target_sha["$display_name"]}" ] && echo true || echo false )
  done
}

get_git_compare_link() {
  service=$1
  source_env=$2
  target_env=$3

  local github_repo=${repo["$service"]}
  local source_sha=${source_sha["$service"]}
  local target_sha=${target_sha["$service"]}

  # Create appropriate GitHub link based on available commit info
  if [ "$target_sha" != "none" ]; then
    if [ "$source_sha" != "$target_sha" ]; then
      echo "https://github.com/$github_repo/compare/${target_sha}...${source_sha}"
    else
      echo ""  # Skip meaningless comparison when SHAs are identical
    fi
  else
    echo "https://github.com/$github_repo/commit/${source_sha}"
  fi
}

show_slack_message() {
  local service=$1
  local source_env=$2
  local target_env=$3
  local backup_id=${4:-""}

  local compare_link=$(get_git_compare_link "$service" "$source_env" "$target_env")
  local channel=${GPP_SLACK_CHANNEL:-"#gpp"}
  local timestamp=$(date -u "+%Y-%m-%d %H:%M:%S UTC")

  echo "  Slack notification to $channel:"
  echo "    $service deployed from $source_env → $target_env"

  if [ -n "$compare_link" ]; then
    echo "    Changes: $compare_link"
  fi

  # Include backup ID for ODB
  if [ ${backup["$service"]} = true ] && [ -n "$backup_id" ] && [ "$backup_id" != "unknown" ]; then
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

  local compare_link=$(get_git_compare_link "$service" "$source_env" "$target_env")
  local timestamp=$(date -u "+%Y-%m-%d %H:%M:%S UTC")

  if [ "$SKIP_SLACK" = true ]; then
    echo "  ! Slack notifications disabled with --skip-slack flag"
    show_slack_message "$service" "$source_env" "$target_env" "$backup_id"
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

## TODO sync hasura instances, we always get that changes are needed
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
promote_heroku_docker_images() {
  local -a systems=("${@:1}")
  
  # Login to Heroku container registry once (for all images)
  if ! heroku container:login; then
    echo "  ! Failed to login to Heroku container registry"
    exit 1
  fi
  
  # Process each entry in the array
  for display_name in "${systems[@]}"; do
    # Check if this service should be promoted
    if [ "${promote["$display_name"]}" = true ]; then
      echo "## Promote ${display_name} to $TARGET_ENV"
      
      # Get image name from the associative array
      local base_name="${image_name["${display_name}"]}-${TARGET_ENV}"

      echo $base_name

      local do_backup_var="backup_${display_name}"
      local backup_id="omited"
      if [ "${!do_backup_var}" = true ]; then
        echo "Capturing database backup before ${display_name} promotion..."
        local backup_output=$(heroku pg:backups:capture --app ${base_name} 2>&1)
        local backup_id=$(echo "$backup_output" | grep -o 'b[0-9]\{3\}' | head -1 || echo "unknown")
        if [ "$backup_id" != "unknown" ]; then
          echo "${display_name} database backup created: ${backup_id}"
        else
          echo "${display_name} database backup created (ID not captured)"
        fi
      fi
      
      declare -a proc_types=() # Split space-separated string into array
      read -a proc_types <<< "${process_types["${display_name}"]}"

      # Release all process types for this image
      for process_type in "${proc_types[@]}"; do
        echo "  Processing ${base_name}/${process_type}"

        local docker_image_sha=$(echo "${docker_image_shas_object["$display_name"]}" | jq -r '.'${process_type}' // "none"' || echo "none")
        if [ "$docker_image_sha" = "none" ]; then
          echo "  ! Docker image SHA for ${base_name}/${process_type} in $SOURCE_ENV is not set - cannot proceed."
          exit 1
        fi

        echo "    Docker image to promote: $docker_image_sha"

        # We use API instead of CLI in order to be able to release a docker image with a specific id.
        local curl_opts=("-s" "--netrc" "-H" "Content-Type: application/json" "-H" "Accept: application/vnd.heroku+json; version=3.docker-releases")

        local release_proc_type=$(curl "${curl_opts[@]}" -X PATCH https://api.heroku.com/apps/${base_name}/formation \
          -d "{
          \"updates\": [
            {
              \"type\": \"${process_type}\",
              \"docker_image\": \"${docker_image_sha}\"
            }
          ]
        }" || echo "error")

        if [ "$release_proc_type" = "error" ]; then
          echo "  ! Error promoting ${base_name}/${process_type} to $TARGET_ENV"
          exit 1
        else
          echo "  ✓ ${base_name}/${process_type} promoted successfully to $TARGET_ENV"
        fi
      done

      echo "  Recording deployment to GitHub..."
      record_github_deployment "$display_name"

      send_slack_notification "$display_name" "$SOURCE_ENV" "$TARGET_ENV" "$backup_id"

      echo
    else
      echo "## Skipping ${display_name} promotion (no changes)"
      echo
    fi
  done
}

# Declare variables

PROMOTE_HASURA=false

declare -A repo
declare -A image_name
declare -A process_types
declare -A backup
declare -A source_sha
declare -A docker_image_shas_object
declare -A target_sha
declare -A promote

repo["Explore"]="gemini-hlsw/explore"

repo["SSO"]="gemini-hlsw/lucuma-odb"
image_name["SSO"]="lucuma-sso"
process_types["SSO"]="web"
backup["SSO"]=true

repo["ITC"]="gemini-hlsw/lucuma-itc"
image_name["ITC"]="itc"
process_types["ITC"]="web"
backup["ITC"]=false

repo["ODB"]="gemini-hlsw/lucuma-odb"
image_name["ODB"]="lucuma-postgres-odb"
process_types["ODB"]="web calibration obscalc"
backup["ODB"]=true

echo "##### Checking for changes between $SOURCE_ENV and $TARGET_ENV"
echo

set_system_vars "${all_systems[@]}"

if check_hasura_changes; then
  PROMOTE_HASURA=true
fi

# remove
promote["ITC"]=true

echo

if [ "$promote["SSO"]" = false ] && [ "$promote["ITC"]" = false ] && [ "$promote["ODB"]" = false ] && [ "$PROMOTE_HASURA" = false ] && [ "$promote["Explore"]" = false ]; then
  echo "##### No changes detected - nothing to promote!"
  echo "All services are already up to date between $SOURCE_ENV and $TARGET_ENV"
  exit 0
fi

# Show what will be promoted
echo "##### Summary of services to promote:"
for display_name in "${all_systems[@]}"; do
  [ "${promote["$display_name"]}" = true ] && echo "  • $display_name will be promoted"
done
[ "$PROMOTE_HASURA" = true ] && echo "  • Hasura migrations will be applied"
echo

if [ "$DRY_RUN" = true ]; then
  echo "##### DRY RUN MODE - No actual promotions will be performed"
  echo
  echo "##### Slack notifications that would be sent:"
  for display_name in "${all_systems[@]}"; do
    [ "${promote["$display_name"]}" = true ] && show_slack_message "$display_name" "$SOURCE_ENV" "$TARGET_ENV"
  done
  exit 0
fi

echo "##### Promoting services from $SOURCE_ENV to $TARGET_ENV"
if [ "$SKIP_SLACK" = true ]; then
  echo "Slack notifications: DISABLED (--skip-slack flag)"
else
  echo "Slack notifications: ENABLED"
fi
echo

promote_heroku_docker_images "${docker_systems[@]}"

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
if [ "${promote["Explore"]}" = true ]; then
  echo "## Promote Explore from $SOURCE_ENV to $TARGET_ENV"

  source_deploy_env=$(map_firebase_deploy_env "$SOURCE_ENV")
  target_deploy_env=$(map_firebase_deploy_env "$TARGET_ENV")
  if firebase hosting:clone explore-gemini-$source_deploy_env:live explore-gemini-$target_deploy_env:live; then
    echo "  ✓ Explore promoted successfully"
  else
    echo "  ! Explore promotion FAILED"
    exit 1
  fi
  echo "  Recording deployment to GitHub..."
  record_github_deployment "Explore"
else
  echo "## Skipping Explore promotion (no changes)"
  echo
fi

# Final summary
echo "##### Promotion completed!"
echo "Services promoted:"
for display_name in "${all_systems[@]}"; do
  [ "${promote["$display_name"]}" = true ] && echo "  ✓ $display_name"
done
[ "$PROMOTE_HASURA" = true ] && echo "  ✓ Hasura migrations"
echo "Services skipped (no changes):"
for display_name in "${all_systems[@]}"; do
  [ "${promote["$display_name"]}" = false ] && echo "  ✗ $display_name"
done
[ "$PROMOTE_HASURA" = false ] && echo "  ✗ Hasura migrations"
