#!/bin/bash

# === CONFIGURATION ===
SCENARIO="reference"  # ← set scenario name here

BASE_LOCAL_DIR="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/meteo"
SCENARIO_DIR="$BASE_LOCAL_DIR/$SCENARIO"  # Full path to local scenario directory

ZIP_NAME="${SCENARIO}_sdbc.zip"

REMOTE_USER="nydegger"
REMOTE_HOST="hyperion.wsl.ch"
REMOTE_DIR="/home/nydegger/Rheinblick/meteo/${SCENARIO}"  # Root dir where the scenario already exists

# === STEP 1: FIND & ZIP ===
cd "$SCENARIO_DIR" || exit 1

echo "[INFO] Zipping sdbc*.2km files in: $SCENARIO"
find . -type f -name "sdbc*.2km" | zip -@ "$ZIP_NAME"

# === STEP 2: SCP TO REMOTE ===
echo "[INFO] Copying $ZIP_NAME to $REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR"
scp "$ZIP_NAME" "$REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR"

# === STEP 3: UNZIP ON REMOTE ===
echo "[INFO] Unzipping on remote server..."
ssh "$REMOTE_USER@$REMOTE_HOST" "cd $REMOTE_DIR && unzip -o $ZIP_NAME && rm $ZIP_NAME"

# === CLEANUP LOCAL ZIP ===
#rm "$ZIP_NAME"
echo "Transfer complete and files unzipped remotely under $REMOTE_DIR."