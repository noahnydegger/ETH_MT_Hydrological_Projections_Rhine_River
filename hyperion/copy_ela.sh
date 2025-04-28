#!/bin/bash

# Inputs
syy=$7
glmask=$8
eyy=$(expr $syy + 35)

src_dir="/home/HyVBigData/data/glaciers/${glmask}"
dest_dir="${SWISS_DIR_OUT}/${RUN_NAME}/${ezg}"

# Round syy down to nearest multiple of 5
syy_rounded=$(( syy - (syy % 5) ))

mkdir -p "$dest_dir"

if [[ "$syy" -eq 2130 ]]; then
    # Special: copy ela2100 rename to ela2130
    cp "${src_dir}/${ezg}-ela2100.bin" "${dest_dir}/${ezg}-ela2130.bin"
else
    for ((year=syy_rounded; year<=eyy && year<=2100; year+=5)); do
        cp "${src_dir}/${ezg}-ela${year}.bin" "${dest_dir}/"
    done
fi