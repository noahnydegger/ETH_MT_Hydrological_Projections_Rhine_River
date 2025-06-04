#!/bin/bash

# Set the destination folder
DEST_FOLDER="swissalti3d_data"
DEST_FOLDER="/Users/noahnydegger/QGIS/ETH/MT_Hydrological_Projections_Rhine_River/swissAlti3d"

# Create destination folder if it doesn't exist
mkdir -p "$DEST_FOLDER"

# STAC API endpoint for swissALTI3D tiles
STAC_ITEMS_URL="http://explorer.swissdatacube.org:8080/stac/collections/swissalti3d/items?limit=10000"

echo "Fetching tile URLs from STAC API..."

# Get all tile URLs from the STAC API and download them
curl -s "$STAC_ITEMS_URL" | jq -r '.features[].assets.data.href' | while read -r URL; do
    echo "Downloading: $URL"
    wget -nc -P "$DEST_FOLDER" "$URL"
done

echo "✅ Download complete. Files saved in: $DEST_FOLDER"