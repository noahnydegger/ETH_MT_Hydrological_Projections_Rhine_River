#!/bin/bash

# === CONFIGURATION ===
SCENARIO="L_2033"    # e.g., reference, rcp85
ENSEMBLES=$( seq 1 8)    # e.g., 1 2 3 4 or $(seq 1 8)
EZG=               # Set to specific EZG (e.g., "Rhb200"), or leave empty to copy all

RUN_TYPE="future_V1" 

REMOTE_USER="nydegger"
REMOTE_HOST="hyperion.wsl.ch"
REMOTE_BASE="/home/nydegger/Rheinblick/R_KNMI"
LOCAL_BASE="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/raw_prevah_output/R_KNMI_${RUN_TYPE}"

# === MAIN LOOP ===
for ENS in $ENSEMBLES; do
    ENS_NAME="${SCENARIO}_ens${ENS}"
    REMOTE_ENS_DIR="${REMOTE_BASE}/${ENS_NAME}"
    LOCAL_ENS_DIR="${LOCAL_BASE}/${ENS_NAME}"

    echo "[INFO] Syncing $ENS_NAME..."

    if [ -n "$EZG" ]; then
        REMOTE_PATH="${REMOTE_ENS_DIR}/${EZG}/"
        LOCAL_PATH="${LOCAL_ENS_DIR}/${EZG}/"
        mkdir -p "$LOCAL_PATH"
        rsync -avz \
            --include='*/' \
            --include='*.mit' \
            --include='*.stats' \
            --include='*.pri' \
            --exclude='*' \
            "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}" "$LOCAL_PATH"
    else
        rsync -avz \
            --include='*/' \
            --include='*.mit' \
            --include='*.stats' \
            --include='*.pri' \
            --exclude='*' \
            "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_ENS_DIR}/" "$LOCAL_ENS_DIR/"
    fi

    if [ $? -eq 0 ]; then
        echo "Successfully copied: $ENS_NAME"
    else
        echo "Error copying: $ENS_NAME"
    fi
done