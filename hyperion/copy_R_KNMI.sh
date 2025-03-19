#!/bin/bash

# Define the remote and local directories
REMOTE_DIR="nydegger@hyperion.wsl.ch:/home/nydegger/Rheinblick/R_KNMI"
LOCAL_DIR="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/raw_prevah_output/R_KNMI"

# Use rsync to copy the entire directory recursively, including only specific file types (.mit, .stats, .pri)
rsync -avz --ignore-existing \
    --include='*/' \
    --include='*.mit' \
    --include='*.stats' \
    --include='*.pri' \
    --exclude='*' \
    $REMOTE_DIR/ $LOCAL_DIR/

# Check if the command was successful
if [ $? -eq 0 ]; then
    echo "Files copied successfully, skipping existing files."
else
    echo "Error occurred during the copy process."
fi