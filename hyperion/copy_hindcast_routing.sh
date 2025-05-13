#!/bin/bash

# === CONFIGURATION ===
# Alpenrhein200:  Alpenrhein200.dat, Alpenrhein200_CTRL_RUN_WSL_F_2021_g73.dat
# Birs200:        Birs200.dat, Birs200_CTRL_RUN_WSL_F_2021_g73.dat
# Bielersee200:   Bielersee200.dat, Bielersee200_CTRL_RUN_WSL_F_2021_g73.dat
# Emme200:        Emme200.dat, Emme200_CTRL_RUN_WSL_F_2021_g73.dat
# Limmat200:      Limmat200.dat, Limmat200_CTRL_RUN_WSL_F_2021_g73.dat
# Swissrhine200:  Swissrhine200_CTRL_RUN_WSL_F_2021_g73.dat
# Reuss200:       Reuss200.dat, Reuss200_CTRL_RUN_WSL_F_2021_g73.dat
# Saane200:       Saane200.dat, Saane200_CTRL_RUN_WSL_F_2021_g73.dat
# ToessGlatt200:  ToessGlatt200.dat, ToessGlatt200_CTRL_RUN_WSL_F_2021_g73.dat
# Thunersee200:   Thunersee200.dat, Thunersee200_CTRL_RUN_WSL_F_2021_g73.dat
# Thur200:        Thur200.dat, Thur200_CTRL_RUN_WSL_F_2021_g73.dat
# Wigger200:      Wigger200_CTRL_RUN_WSL_F_2021_g73.dat

FILE_NAMES=(
Alpenrhein200.dat
Alpenrhein200_CTRL_RUN_WSL_F_2021_g73.dat
Birs200.dat
Birs200_CTRL_RUN_WSL_F_2021_g73.dat
Bielersee200.dat
Bielersee200_CTRL_RUN_WSL_F_2021_g73.dat
Emme200.dat
Emme200_CTRL_RUN_WSL_F_2021_g73.dat
Limmat200.dat
Limmat200_CTRL_RUN_WSL_F_2021_g73.dat
Swissrhine200_CTRL_RUN_WSL_F_2021_g73.dat
Reuss200.dat
Reuss200_CTRL_RUN_WSL_F_2021_g73.dat
Saane200.dat
Saane200_CTRL_RUN_WSL_F_2021_g73.dat
ToessGlatt200.dat
ToessGlatt200_CTRL_RUN_WSL_F_2021_g73.dat
Thunersee200.dat
Thunersee200_CTRL_RUN_WSL_F_2021_g73.dat
Thur200.dat
Thur200_CTRL_RUN_WSL_F_2021_g73.dat
Wigger200_CTRL_RUN_WSL_F_2021_g73.dat
)

REMOTE_USER="nydegger"
REMOTE_HOST="hyperion.wsl.ch"
REMOTE_BASE="/home/zappa/nfp61/run/CHBILANZ/verif/archive/sim_23"
LOCAL_BASE="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/raw_prevah_output/routing_hindcast/CHBILANZ"

# === SETUP ===
mkdir -p "$LOCAL_BASE"
failed_files=()

echo "[INFO] Starting file copy for ${#FILE_NAMES[@]} routing output files..."
echo "[INFO] Copying from ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_BASE} to ${LOCAL_BASE}"

# === MAIN LOOP ===
for FILE_NAME in "${FILE_NAMES[@]}"; do
    REMOTE_FILE="${REMOTE_BASE}/${FILE_NAME}"
    LOCAL_FILE="${LOCAL_BASE}/${FILE_NAME}"

    rsync -az "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_FILE}" "$LOCAL_FILE"

    if [ $? -ne 0 ]; then
        failed_files+=("$FILE_NAME")
    fi
done

# === SUMMARY OUTPUT ===
if [ ${#failed_files[@]} -eq 0 ]; then
    echo "[INFO] All files copied successfully."
else
    echo "[ERROR] Failed to copy the following files:"
    for file in "${failed_files[@]}"; do
        echo "  - $file"
    done
fi
