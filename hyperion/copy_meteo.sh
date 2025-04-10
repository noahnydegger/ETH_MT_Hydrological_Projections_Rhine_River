#!/bin/bash

# Define the base remote and local directories
REMOTE_BASE="nydegger@hyperion.wsl.ch:/home/nydegger/Rheinblick/meteo"
LOCAL_BASE="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/meteo"

# Allow specifying ens and scenario (use provided arguments or defaults)
SCENARIO="${2:-Hn_2100}"  # Default to 'reference' if not provided

# Define full paths for remote and local directories
REMOTE_DIR="$REMOTE_BASE/$SCENARIO"
LOCAL_DIR="$LOCAL_BASE/$SCENARIO"

# Create the local directory if it doesn't exist
mkdir -p "$LOCAL_DIR"

# Define the year range
start_year=2086
end_year=2115

for ENS in ens{1..8}; do
    for ((YEAR=start_year; YEAR<=end_year; YEAR++)); do
        REMOTE_DIR_YEAR="$REMOTE_DIR/$ENS/Full/$YEAR"
        LOCAL_DIR_YEAR="$LOCAL_DIR/$ENS/Full/$YEAR"
        # Create the local directory if it doesn't exist
        mkdir -p "$LOCAL_DIR_YEAR"
        rsync -avzL \
            --include='*/' \
            --include="*prec*.2km" \
            --include="*tair*.2km" \
            --include="*radg*.2km" \
            --include="*sund*.2km" \
            --include="*rhum*.2km" \
            --include="*wspd*.2km" \
            --exclude="*" \
            "$REMOTE_DIR_YEAR/" "$LOCAL_DIR_YEAR/"
    done
done

# Check if the command was successful
if [ $? -eq 0 ]; then
    echo "Successfully copied files matching '$FILTER' from $REMOTE_DIR to $LOCAL_DIR"
else
    echo "Error occurred during the file transfer."
fi