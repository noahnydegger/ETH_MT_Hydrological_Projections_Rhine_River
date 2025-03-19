#!/bin/bash

# Define the remote and local directories
REMOTE_BASE="nydegger@hyperion.wsl.ch:/home/nydegger/Rheinblick/R_KNMI"
LOCAL_BASE="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/raw_prevah_output/R_KNMI"

# Loop through the range 1 to 8 to handle each reference_ens1 to reference_ens8
for i in {2..8}
do
  # Define the remote and local paths for the current reference_ens
  REMOTE_PATH="$REMOTE_BASE/reference_ens$i"
  LOCAL_PATH="$LOCAL_BASE/reference_ens$i"
  
  # Remove the existing RhB200 and Rhb200 directories if they exist
  rm -rf "$LOCAL_PATH/RhB200"
  rm -rf "$LOCAL_PATH/Rhb200"

  # Copy 'RhB200' and 'Rhb200' directories from the remote to local machine
  scp -r $REMOTE_PATH/RhB200 "$LOCAL_PATH/"
  scp -r $REMOTE_PATH/Rhb200 "$LOCAL_PATH/"

  echo "Copied RhB200 and Rhb200 for reference_ens$i to local_base/reference_ens$i."
done