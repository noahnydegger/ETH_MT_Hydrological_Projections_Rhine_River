#!/bin/bash

# === CONFIGURATION ===
SCENARIO="Hd_2150"        # e.g., reference, rcp85
ENSEMBLES=$(seq 1 8)        # e.g., 1 2 3 4
EZG=                      # Set to e.g. "NoW200", or leave empty to copy all

RUN_TYPE="future_V1"

REMOTE_USER="nydegger"
REMOTE_HOST="hyperion.wsl.ch"
REMOTE_BASE="/home/nydegger/Rheinblick/routing"
LOCAL_BASE="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/raw_prevah_output/routing_${RUN_TYPE}"

# === MAIN LOOP ===
for ENS in $ENSEMBLES; do
    ENS_NAME="${SCENARIO}_ens${ENS}"

    if [ -n "$EZG" ]; then
        REMOTE_PATH="${REMOTE_BASE}/${EZG}/${ENS_NAME}/"
        LOCAL_PATH="${LOCAL_BASE}/${EZG}/${ENS_NAME}/"

        echo "[INFO] Copying $ENS_NAME from EZG: $EZG"
        mkdir -p "$LOCAL_PATH"

        rsync -avz \
            --include='*.dat' \
            --exclude='*' \
            "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}" "$LOCAL_PATH"

        [[ $? -eq 0 ]] && echo "[INFO] Success: $ENS_NAME" || echo "[ERROR] Failed: $ENS_NAME"
    else
        echo "[INFO] Searching for $ENS_NAME in all EZGs..."
        EZG_LIST=$(ssh ${REMOTE_USER}@${REMOTE_HOST} "find ${REMOTE_BASE} -type d -name ${ENS_NAME} -printf '%h\n' | xargs -n1 basename | sort -u")

        for CURRENT_EZG in $EZG_LIST; do
            REMOTE_PATH="${REMOTE_BASE}/${CURRENT_EZG}/${ENS_NAME}/"
            LOCAL_PATH="${LOCAL_BASE}/${CURRENT_EZG}/${ENS_NAME}/"

            echo "[INFO] Copying $ENS_NAME from EZG: $CURRENT_EZG"
            mkdir -p "$LOCAL_PATH"

            rsync -avz \
                --include='*.dat' \
                --exclude='*' \
                "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}" "$LOCAL_PATH"

            [[ $? -eq 0 ]] && echo "[INFO] Success: $ENS_NAME in $CURRENT_EZG" || echo "[ERROR] Failed: $ENS_NAME in $CURRENT_EZG"
        done
    fi
done