#!/bin/bash

# Before running anything:
# I am currently running the script using MSYS2 UCRT64 as my Unix-like development environment for Windows
# You can get it from https://www.msys2.org/
# Required packages (install once with pacman -S packagename): curl, zip
# The script is built in a way that it should be run from inside the analyses folder from the rcompendium structure:
# https://cran.r-project.org/web/packages/rcompendium/index.html
# It doesn't need to be run exactly like that, but paths will need to be changed accordingly

# How to run:
# 1. cd path/to/analyses
# 2. bash script_name.sh

# Occurrence fields downloaded via https://specieslink.net/
# "datelastmodified" "basisofrecord" "coordinateprecision" "county" "daycollected" "dayidentified"
# "notes" "genus" "latitude" "longitude" "monthcollected" "monthidentified" "collector" "collectornumber"
# "scientificnameauthor" "verbatimlatitude" "verbatimlongitude" "yearidentified" "catalognumber" "collectioncode"
# "country" "family" "identifiedby" "institutioncode" "locality" "scientificname" "scientificnameauthor"

# Notes on how speciesLink splits large downloads:
# - The export is split into parts of ~1 GB each (part 1 was 1,073,741,735 bytes); only the LAST part is smaller.
# - Only part 1 has a header line; parts 2..N start directly with records.
# - The server does not send the file size, and seems to close connections after ~10 min,
#   so a slow download can end "successfully" but truncated. This script checks for that and resumes.

# Variables
# The download is split into parts: BASE_LINK-1, BASE_LINK-2, ..., BASE_LINK-N_PARTS
BASE_LINK="https://specieslink.net/search/download/20260921082144-0027476"
N_PARTS=6
MIN_PART_SIZE=1000000000   # every part except the last should be at least this big (~1 GB)
MAX_TRIES=20               # max download/resume attempts per part
OUT_DIR="$(cd "$(dirname "$0")/.." && pwd)/data/raw-data/speciesLink"   # Endemism/data/raw-data/speciesLink (filtered output only)
# Raw parts (~1 GB each) are downloaded outside the project and deleted after filtering;
# another location can be given with: TMP_DIR=/path bash 99_bash_specieslink_occurrence.sh
TMP_DIR="${TMP_DIR:-/c/Users/ggrittz/Desktop/data/speciesLink_data/tmp}"
SPECS_FILE="$(cd "$(dirname "$0")/.." && pwd)/data/derived-data/spp_names.txt"
FILTERED_OUTPUT="$OUT_DIR/speciesLink_occurrences.txt"
FINAL_OUTPUT="$OUT_DIR/speciesLink_occurrences.zip"

# Step 0a: Check that everything needed exists BEFORE downloading anything
for cmd in curl zip; do
  if ! command -v "$cmd" > /dev/null; then
    echo "ERROR: '$cmd' is not installed. Run: pacman -S $cmd" >&2
    exit 1
  fi
done
if [ ! -f "$SPECS_FILE" ]; then
  echo "ERROR: species file not found: $SPECS_FILE" >&2
  exit 1
fi

mkdir -p "$OUT_DIR" "$TMP_DIR"

# Step 0b: Clean the species list
# Removes Windows line endings (\r) and empty lines.
# An empty line in the pattern file would make grep -F match EVERY line.
PATTERNS="$TMP_DIR/spp_names_clean.txt"
tr -d '\r' < "$SPECS_FILE" | sed '/^[[:space:]]*$/d' > "$PATTERNS"

# Downloads one part, resuming until it looks complete:
#   - the file ends with a newline (not cut mid-line), and
#   - for every part except the last, it is at least MIN_PART_SIZE bytes
download_part() {
  local url="$1" out="$2" is_last="$3"
  local attempt=0 rc size ends_nl

  while :; do
    attempt=$((attempt + 1))
    echo "  attempt ${attempt}/${MAX_TRIES}..."
    # -C - resumes from where the previous attempt stopped
    curl -L --fail --retry 3 -C - -o "$out" "$url"
    rc=$?
    if [ "$rc" -eq 33 ]; then
      # Server refused to resume: start this part again from zero
      echo "  server does not support resuming; restarting this part from scratch" >&2
      rm -f "$out"
    fi

    size=$(stat -c %s "$out" 2>/dev/null || echo 0)
    ends_nl=$(tail -c 1 "$out" 2>/dev/null | wc -l)
    echo "  size so far: ${size} bytes"

    if [ "$ends_nl" -eq 1 ] && { [ "$is_last" = yes ] || [ "$size" -ge "$MIN_PART_SIZE" ]; }; then
      return 0
    fi
    if [ "$attempt" -ge "$MAX_TRIES" ]; then
      echo "ERROR: $url still looks incomplete after ${MAX_TRIES} attempts (${size} bytes)." >&2
      return 1
    fi
    echo "  looks incomplete - resuming..."
    sleep 5
  done
}

: > "$FILTERED_OUTPUT"  # Start with an empty output file
HEADER=""

for i in $(seq 1 "$N_PARTS"); do
  PART_LINK="${BASE_LINK}-${i}"
  PART_FILE="$TMP_DIR/part_${i}.txt"
  IS_LAST=no; [ "$i" -eq "$N_PARTS" ] && IS_LAST=yes

  # Step 1: Download this part (with resume + completeness check)
  echo "Downloading part ${i}/${N_PARTS}..."
  rm -f "$PART_FILE"
  if ! download_part "$PART_LINK" "$PART_FILE" "$IS_LAST"; then
    echo "Stopping. Partial results kept at: $FILTERED_OUTPUT" >&2
    exit 1
  fi

  # Step 2: Header - only part 1 has one. For other parts, skip the first line only if it IS the header.
  FIRST_LINE=$(head -n 1 "$PART_FILE" | tr -d '\r')
  if [ "$i" -eq 1 ]; then
    HEADER="$FIRST_LINE"
    echo "$HEADER" > "$FILTERED_OUTPUT"
    START_LINE=2
  elif [ "$FIRST_LINE" = "$HEADER" ]; then
    START_LINE=2
  else
    START_LINE=1   # no header in this part: keep its first line, it's a record
  fi

  # Step 3: Filter this part and append
  N_BEFORE=$(wc -l < "$FILTERED_OUTPUT")
  tail -n +"$START_LINE" "$PART_FILE" | grep -F -f "$PATTERNS" >> "$FILTERED_OUTPUT"
  N_AFTER=$(wc -l < "$FILTERED_OUTPUT")
  echo "Part ${i}: $((N_AFTER - N_BEFORE)) records kept"

  # Remove the raw part right away to save disk space
  rm "$PART_FILE"
done

# Step 4: Sanity check - every line should have the same number of tab-separated columns
echo "Column check (number of lines | number of columns). Ideally a single row:"
awk -F'\t' '{print NF}' "$FILTERED_OUTPUT" | sort -n | uniq -c

# Step 5: Zip the final output
# -j stores only the file name inside the zip (not the full c/Users/... path)
# The .txt is kept: it is read by 01_prepare_splink_data.R
rm -f "$FINAL_OUTPUT"
if zip -j "$FINAL_OUTPUT" "$FILTERED_OUTPUT"; then
  echo "Process completed. Total records: $((N_AFTER - 1)). Outputs: $FILTERED_OUTPUT and $FINAL_OUTPUT"
else
  echo "ERROR: zip failed. The unzipped file was kept at: $FILTERED_OUTPUT" >&2
fi

# Cleanup
rm -f "$PATTERNS"
rmdir "$TMP_DIR" 2>/dev/null