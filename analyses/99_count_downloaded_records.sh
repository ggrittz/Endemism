#!/usr/bin/env bash
# Counts the records (lines minus header) of the occurrence files already
# filtered by our list of names, and saves them as manuscript numbers
# (merged by 12_manuscript_numbers.R).
#
# Run from MSYS2 UCRT64, from inside the "analyses" folder:
#   bash 99_count_downloaded_records.sh

set -euo pipefail

DATA_DIR="../data/raw-data"
OUT_DIR="../outputs/numbers"
OUT_FILE="$OUT_DIR/00_downloaded_records.csv"

# source name and its filtered file
SOURCES=("gbif" "jabot" "reflora" "specieslink")
FILES=("$DATA_DIR/GBIF/filtered_occurrence.txt"
       "$DATA_DIR/JABOT/jabot_occurrence_filtered.txt"
       "$DATA_DIR/REFLORA/reflora_occurrence_filtered.txt"
       "$DATA_DIR/speciesLink/speciesLink_occurrence_filtered.txt")

for f in "${FILES[@]}"; do
  [[ -f "$f" ]] || { echo "ERROR: $f not found" >&2; exit 1; }
done

mkdir -p "$OUT_DIR"
echo '"key","value","description"' > "$OUT_FILE"

total=0
for i in "${!SOURCES[@]}"; do
  n=$(( $(wc -l < "${FILES[$i]}") - 1 ))
  total=$(( total + n ))
  echo "${SOURCES[$i]}: $n"
  echo "\"n_records_download_${SOURCES[$i]}\",$n,\"Records in the ${SOURCES[$i]} file filtered by our list of names\"" >> "$OUT_FILE"
done

echo "total: $total"
echo "\"n_records_download_total\",$total,\"Records in all source files filtered by our list of names\"" >> "$OUT_FILE"
