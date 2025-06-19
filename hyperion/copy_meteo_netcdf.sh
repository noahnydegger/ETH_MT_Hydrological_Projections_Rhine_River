#!/bin/bash

# Define remote and local base paths
REMOTE_USER="nydegger"
REMOTE_HOST="hyperion.wsl.ch"
REMOTE_BASE="/storage/HyVBigData/KNMI23_data/KNMI_cutproj/netcdf_processed"
LOCAL_BASE="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/meteo/netcdf_processed"

# Define the list of variables and corresponding local folders
VARIABLES=("pr")
LOCAL_FOLDERS=("precipitation")

# Define file suffix
SUFFIX=".nc"

# Allow specifying scenario and year/member filters (optional)
SCENARIO="${1:-*}"
HORIZON="${2:-*}"
MEMBER="${3:-*}"

for i in "${!VARIABLES[@]}"; do
    VAR="${VARIABLES[$i]}"
    FOLDER="${LOCAL_FOLDERS[$i]}"

    REMOTE_DIR="${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_BASE}"
    
    if [ "$SCENARIO" == "*" ]; then
        LOCAL_DIR="$LOCAL_BASE/$FOLDER/all_scenarios"
    else
        LOCAL_DIR="$LOCAL_BASE/$FOLDER/$SCENARIO"
    fi

    # Create local directory
    mkdir -p "$LOCAL_DIR"

    # Build pattern
    PATTERN="*${VAR}*${SCENARIO}*${HORIZON}*ens${MEMBER}*${SUFFIX}"

    # Sync files matching pattern
    rsync -avzL \
        --include='*/' \
        --include="$PATTERN" \
        --exclude="*" \
        "$REMOTE_DIR/" "$LOCAL_DIR/"
done

# Check final command status
if [ $? -eq 0 ]; then
    echo "Successfully copied variable-specific files to $LOCAL_BASE"
else
    echo "An error occurred during the file transfer."
fi