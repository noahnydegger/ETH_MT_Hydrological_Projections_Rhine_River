#!/bin/bash

# Define the directory
output_dir="date_txt_files"

# Create the directory if it doesn't exist
mkdir -p "$output_dir"

# Define all chains
chains=(
Hd_2050 Hd_2100 Hd_2150
Hn_2050 Hn_2100 Hn_2150
L_2033
Ld_2100
Ln_2100
Md_2050 Md_2100 Md_2150
Mn_2050 Mn_2100 Mn_2150
reference
)

# Define matching iyy values
iyy=(
2030 2080 2130
2030 2080 2130
2013
2080
2080
2030 2080 2130
2030 2080 2130
1985
)

# Loop through chains and create rdate and edate files
for i in "${!chains[@]}"; do
  chain_name="${chains[$i]}"
  year_value="${iyy[$i]}"

  rdate="${year_value}1001"
  edate="$((year_value + 35))1231"

  echo "$rdate" > "${output_dir}/${chain_name}_rdate.txt"
  echo "$edate" > "${output_dir}/${chain_name}_edate.txt"
done