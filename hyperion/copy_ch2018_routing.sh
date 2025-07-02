#!/bin/bash

FILE_NAMES=(
Alpenrhein200
Birs200
Bielersee200
Emme200
Limmat200
# Swissrhine200
Reuss200
Saane200
ToessGlatt200
Thunersee200
Thur200
Wigger200
)

REMOTE_USER="nydegger"
REMOTE_HOST="hyperion.wsl.ch"
REMOTE_BASE="/home/zappa/nfp61/run/R_CH2018/routing"
LOCAL_BASE="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/raw_prevah_output/routing_CH2018"

# === SETUP ===
mkdir -p "$LOCAL_BASE"

echo "[INFO] Starting file copy for ${#FILE_NAMES[@]} routing output files..."
echo "[INFO] Copying from ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_BASE} to ${LOCAL_BASE}"

INCLUDE_PATTERNS=()
for NAME in "${FILE_NAMES[@]}"; do
    INCLUDE_PATTERNS+=("--include=*/${NAME}*.dat")
done

echo "[INFO] Include patterns:"
for pattern in "${INCLUDE_PATTERNS[@]}"; do
    echo "  $pattern"
done
echo "[INFO] Running rsync..."

rsync -az \
    "${INCLUDE_PATTERNS[@]}" \
    --include="*/" \
    --exclude="*" \
    "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_BASE}/" \
    "$LOCAL_BASE"

echo "[INFO] rsync completed."
