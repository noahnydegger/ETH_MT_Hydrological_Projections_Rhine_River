#!/bin/bash

# Define the base remote and local directories
REMOTE_BASE="nydegger@hyperion.wsl.ch:/home/nydegger/Rheinblick/meteo"
LOCAL_BASE="/Volumes/MT_case_sensitive/ETH_MT_Hydrological_Projections_Rhine_River/Data/Rheinblick2027/meteo"

# Allow specifying ens and scenario (use provided arguments or defaults)
SCENARIO="${2:-reference}"  # Default to 'reference' if not provided

# Define full paths for remote and local directories
REMOTE_DIR="$REMOTE_BASE/$SCENARIO"
LOCAL_DIR="$LOCAL_BASE/$SCENARIO"

# Allow filtering filenames (optional)
FILTER="${3:-*.2km}"  # Default filter to "*.2km" files if no filter is provided

# Create the local directory if it doesn't exist
mkdir -p "$LOCAL_DIR"

# Define the year range and variable list
YEAR_RANGE="1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020"
# 1991 
# Copy only files that contain "radg" or "sund" in the filename
for ENS in ens{5..8}; do
    for YEAR in $YEAR_RANGE; do
    REMOTE_DIR_YEAR="$REMOTE_DIR/$ENS/Full/$YEAR"
    LOCAL_DIR_YEAR="$LOCAL_DIR/$ENS/Full/$YEAR"
    # Create the local directory if it doesn't exist
    mkdir -p "$LOCAL_DIR_YEAR"
    rsync -avzL \
    --include='*/' \
    --include="*prec*.2km" \
    --include="*tair*.2km" \
    --include="*radg*.2km" \
    --include="*sund*.2km" \
    --exclude="*" \
    "$REMOTE_DIR_YEAR/" "$LOCAL_DIR_YEAR/"
    done
done

# Check if the command was successful
if [ $? -eq 0 ]; then
    echo "Successfully copied files matching '$FILTER' from $REMOTE_DIR to $LOCAL_DIR"
else
    echo "Error occurred during the file transfer."
fi