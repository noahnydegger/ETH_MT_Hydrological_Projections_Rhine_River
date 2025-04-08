#!/bin/bash

# Define the remote and local directories
REMOTE_DIR="nydegger@hyperion.wsl.ch:/home/nydegger/Rheinblick/R_KNMI"
LOCAL_DIR="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/raw_prevah_output/R_KNMI"

# SSH into the remote server and find all matching folders
folders=$(ssh nydegger@hyperion.wsl.ch "find /home/nydegger/Rheinblick/R_KNMI -type d -name 'Hd_2100_*'")

# Loop through each folder and copy its content
for folder in $folders; do
    # Extract the folder name (e.g., reference_ens1, reference_ens2)
    folder_name=$(basename "$folder")
    
    # Define the corresponding local folder
    local_folder="$LOCAL_DIR/$folder_name"

    # Delete the local folder if it exists
    if [ -d "$local_folder" ]; then
        echo "Deleting existing folder: $local_folder"
        rm -rf "$local_folder"
    fi

    # Copy content from remote to local
    rsync -avz \
        --include='*/' \
        --include='*.mit' \
        --include='*.stats' \
        --include='*.pri' \
        --exclude='*' \
        "nydegger@hyperion.wsl.ch:$folder/" "$local_folder/"

    # Check if the command was successful
    if [ $? -eq 0 ]; then
        echo "Successfully copied: $folder -> $local_folder"
    else
        echo "Error occurred copying: $folder"
    fi
done