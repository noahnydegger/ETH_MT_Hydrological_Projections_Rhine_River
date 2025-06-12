#!/bin/bash

# Parent directory (you can set this to a specific path)
PARENT_DIR="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Plots/future_V1/Rhb_RhB"  # current directory

# Output directories
RHB_DIR="$PARENT_DIR/RhB"
Rhb_DIR="$PARENT_DIR/Rhb"

# Create output directories
mkdir -p "$RHB_DIR"
mkdir -p "$Rhb_DIR"

# Find and copy files with 'RhB200' in their name
find "$PARENT_DIR" -type f -name '*RhB200*' | while read -r FILE; do
    # Compute relative path
    REL_PATH="${FILE#$PARENT_DIR/}"
    REL_DIR="$(dirname "$REL_PATH")"

    # Create corresponding directory structure inside RhB
    mkdir -p "$RHB_DIR/$REL_DIR"

    # Copy file
    cp "$FILE" "$RHB_DIR/$REL_DIR/"
done

# Find and copy files with 'Rhb200' in their name
find "$PARENT_DIR" -type f -name '*Rhb200*' | while read -r FILE; do
    # Compute relative path
    REL_PATH="${FILE#$PARENT_DIR/}"
    REL_DIR="$(dirname "$REL_PATH")"

    # Create corresponding directory structure inside Rhb
    mkdir -p "$Rhb_DIR/$REL_DIR"

    # Copy file
    cp "$FILE" "$Rhb_DIR/$REL_DIR/"
done

echo "Done!"