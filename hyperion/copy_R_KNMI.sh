#!/bin/bash

# Define the remote and local directories
REMOTE_DIR="nydegger@hyperion.wsl.ch:/home/nydegger/Rheinblick/R_KNMI"
LOCAL_DIR="/Users/noahnydegger/GitHub/ETH/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/raw_prevah_output/R_KNMI"

# Use rsync to copy the entire directory recursively, skipping existing files
rsync -avz --ignore-existing $REMOTE_DIR/ $LOCAL_DIR/

# Check if the command was successful
if [ $? -eq 0 ]; then
    echo "Directory copied successfully, skipping existing files."
else
    echo "Error occurred during the copy process."
fi