#!/bin/bash

DRY_RUN=${1:-}

# Function to check Firebase hosting changes
check_firebase_changes() {
  local source_project=$1
  local target_project=$2

  echo "Checking for Firebase hosting changes between $source_project and $target_project..."

  # Get release info for both Firebase projects using releases list
  local source_info=$(firebase hosting:releases:list --project "$source_project" --json 2>/dev/null | jq -r '.[0].name // "none"' || echo "none")
  local target_info=$(firebase hosting:releases:list --project "$target_project" --json 2>/dev/null | jq -r '.[0].name // "none"' || echo "none")

  echo "  Debug: Source release: '$source_info'"
  echo "  Debug: Target release: '$target_info'"

  if [ "$source_info" != "$target_info" ] && [ "$source_info" != "none" ] && [ "$target_info" != "none" ]; then
    echo "  ✓ Firebase hosting has changes (source: $source_info, target: $target_info)"
    return 0  # Changes detected
  elif [ "$source_info" = "none" ] || [ "$target_info" = "none" ]; then
    echo "  ! Could not retrieve Firebase release info - assuming changes needed"
    return 0  # Assume changes when we can't get info
  else
    echo "  ✗ Firebase hosting has no changes (both at release $target_info)"
    return 1  # No changes
  fi
}

# Check backend services
pushd .
source ./promote.sh staging production $DRY_RUN
BACKEND_PROMOTION_RESULT=$?
popd

# Check Firebase hosting changes
PROMOTE_FIREBASE=false
if check_firebase_changes "explore-gemini-stage" "explore-gemini-prod"; then
  PROMOTE_FIREBASE=true
fi

# If no backend changes and no Firebase changes, exit early
if [ $BACKEND_PROMOTION_RESULT -eq 0 ] && [ "$PROMOTE_FIREBASE" = false ]; then
  echo
  echo "##### No Firebase hosting changes detected either - nothing more to promote!"
  exit 0
fi

# Show Firebase promotion plan
echo
if [ "$PROMOTE_FIREBASE" = true ]; then
  echo "##### Firebase hosting changes detected - will promote explore to production"
else
  echo "##### Firebase hosting has no changes - skipping promotion"
fi

if [ "$DRY_RUN" = "dry-run" ]; then
  echo "##### DRY RUN MODE - No Firebase promotion will be performed"
  exit 0
fi

# Execute Firebase promotion if needed
if [ "$PROMOTE_FIREBASE" = true ]; then
  echo
  echo "## Promote explore Firebase hosting to production"
  firebase hosting:clone explore-gemini-stage:live explore-gemini-prod:live
  echo "  ✓ Firebase hosting promoted successfully"
else
  echo
  echo "## Skipping Firebase hosting promotion (no changes)"
fi

echo
echo "##### All promotions completed!"
