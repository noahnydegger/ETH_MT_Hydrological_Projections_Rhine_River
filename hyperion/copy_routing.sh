#!/bin/bash

# Define the remote and local directories
REMOTE_DIR="nydegger@hyperion.wsl.ch:/home/nydegger/Rheinblick/routing/Thu200"
LOCAL_DIR="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/raw_prevah_output/routing/Thu200"

# SSH into the remote server and find all subfolders
folders=$(ssh nydegger@hyperion.wsl.ch "find /home/nydegger/Rheinblick/routing/Thu200 -mindepth 1 -maxdepth 1 -type d")

# Loop through each folder and copy its content
for folder in $folders; do
    # Extract the folder name (e.g., folder1, folder2)
    folder_name=$(basename "$folder")
    
    # Define the corresponding local folder
    local_folder="$LOCAL_DIR/$folder_name"

    # Delete the local folder if it exists
    if [ -d "$local_folder" ]; then
        echo "Deleting existing folder: $local_folder"
        rm -rf "$local_folder"
    fi

    # Copy content from remote to local
    rsync -avz "nydegger@hyperion.wsl.ch:$folder/" "$local_folder/"

    # Check if the command was successful
    if [ $? -eq 0 ]; then
        echo "Successfully copied: $folder -> $local_folder"
    else
        echo "Error occurred copying: $folder"
    fi
done