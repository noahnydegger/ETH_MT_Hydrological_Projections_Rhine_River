#!/bin/bash

# === CONFIGURATION ===
# AlpenRhine: 9999 2288 2150 2387 2355 2033 2602 2473
# Birs:       2106 2202
# Bielersee:  2016 2450 2063 2029 2307 2034
# Emme:       2155 2070
# Limmat:     2243 2099 2104 2372 2426 2176
# SwissRhine: 2205 2091 2289 2143
# Reuss:      2152 2110 2018 2056
# Saane:      2119 2160 2472 2085 2179 2135
# Toessglatt: 2132 2415
# Thunersee:  2030 2109
# Thur:       2044 2181
# Wigger:     2450

obs=(
9999 2288 2150 2387 2355 2033 2602 2473
2106 2202
2016 2450 2063 2029 2307 2034
2155 2070
2243 2099 2104 2372 2426 2176
2205 2091 2289 2143
2152 2110 2018 2056
2119 2160 2472 2085 2179 2135
2132 2415
2030 2109
2044 2181
2450
)

# example path "/home/zappa/nfp61/run/CHBILANZ/verif/runoff/2070.daily.mean.dat"

REMOTE_USER="nydegger"
REMOTE_HOST="hyperion.wsl.ch"
REMOTE_BASE="/home/zappa/nfp61/run/CHBILANZ/verif/runoff"
LOCAL_BASE="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/discharge_measurements/CHBILANZ"

# === SETUP ===
mkdir -p "$LOCAL_BASE"
failed_files=()

echo "[INFO] Starting file copy for ${#obs[@]} observation IDs..."
echo "[INFO] Copying from ${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_BASE} to ${LOCAL_BASE}"

# === MAIN LOOP ===
for ID in "${obs[@]}"; do
    FILE_NAME="${ID}.daily.mean.dat"
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
