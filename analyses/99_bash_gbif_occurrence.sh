#!/usr/bin/env bash

# Filters an already-extracted GBIF occurrence.txt by a species list and keeps
# only selected columns, then zips the result.
#
# Run from MSYS2 UCRT64, from inside the rcompendium "analyses" folder
# (or edit the relative paths below).
#   chmod +x filter_gbif_occurrence.sh   (once)
#   ./filter_gbif_occurrence.sh
#
# Needs: gawk, zip  ->  pacman -S gawk zip

set -euo pipefail

# ---------------------------------------------------------------- variables --
# The raw download (~700 GB) stays outside the project; only the filtered
# output goes to Endemism/data/raw-data/GBIF (read by 01_prepare_gbif_data.R).
# Another raw location can be given with: OCCURRENCE_FILE=/path/occurrence.txt bash 99_bash_gbif_occurrence.sh
# (Windows path C:\Users\... is written as /c/Users/... inside MSYS2)
OCCURRENCE_FILE="${OCCURRENCE_FILE:-/c/Users/ggrittz/Desktop/data/gbif_data/occurrence.txt}"
GBIF_DIR="$(cd "$(dirname "$0")/.." && pwd)/data/raw-data/GBIF"
SPECS_FILE="../data/derived-data/spp_names.txt"   # one name per line (relative to analyses/)
OUTPUT_TXT="$GBIF_DIR/filtered_occurrence.txt"    # read by 01_prepare_gbif_data.R
FINAL_OUTPUT="$GBIF_DIR/final_occurrence.zip"
KEEP_TXT=true                                     # 01_prepare_gbif_data.R reads the .txt

# A row is kept if the name in ANY of these columns matches a name in the list.
# The match ignores authorship: "Myrcia splendens (Sw.) DC." matches "Myrcia splendens".
MATCH_COLUMNS=("species" "scientificName" "acceptedScientificName")

# Columns to keep (output follows this order)
# (not named COLUMNS: bash overwrites that variable with the terminal width)
KEEP_COLUMNS=("acceptedScientificName" "gbifID" "basisOfRecord" "bibliographicCitation" "catalogNumber" "collectionCode"
         "collectionID" "coordinatePrecision" "countryCode" "county" "datasetID" "datasetName" "dateIdentified"
         "day" "decimalLatitude" "decimalLongitude" "eventDate" "family" "fieldNotes" "genus" "habitat"
         "identificationRemarks" "identifiedBy" "institutionCode" "locality" "month" "municipality"
         "occurrenceRemarks" "recordedBy" "recordNumber" "scientificName" "stateProvince" "taxonomicStatus"
         "taxonRank" "type" "typeStatus" "verbatimElevation" "verbatimEventDate" "verbatimLocality" "year")

# ------------------------------------------------------------------- checks --
[[ -f "$OCCURRENCE_FILE" ]] || { echo "ERROR: $OCCURRENCE_FILE not found" >&2; exit 1; }
[[ -s "$SPECS_FILE" ]]      || { echo "ERROR: $SPECS_FILE missing or empty" >&2; exit 1; }
command -v zip >/dev/null   || { echo "ERROR: zip not installed (pacman -S zip)" >&2; exit 1; }
mkdir -p "$(dirname "$OUTPUT_TXT")" "$(dirname "$FINAL_OUTPUT")"

# Byte-wise comparison: much faster on a huge file, and exact matching
# of UTF-8 names still works.
export LC_ALL=C

# --------------------------------------------- filter rows + columns (1 pass) --
FILE_BYTES=$(stat -c %s "$OCCURRENCE_FILE")   # used for the progress bar

awk -v cols="${KEEP_COLUMNS[*]}" -v keys="${MATCH_COLUMNS[*]}" -v tb="$FILE_BYTES" '
# Progress bar on screen (stderr), based on bytes read so far
function progress(   pct, filled, bar, i, el, eta) {
    pct = (tb > 0) ? 100 * bytes / tb : 0
    if (pct > 100) pct = 100
    filled = int(pct / 2.5)
    bar = ""
    for (i = 1; i <= 40; i++) bar = bar (i <= filled ? "#" : "-")
    el  = systime() - t0
    eta = (pct > 0) ? int(el * (100 - pct) / pct) : 0
    printf "\r[%s] %5.1f%%  rows read: %d  kept: %d  ETA: %dm%02ds ",
           bar, pct, total, kept, int(eta / 60), eta % 60 > "/dev/stderr"
    fflush("/dev/stderr")
}
function printrow(   i, line) {
    line = $idx[1]
    for (i = 2; i <= n; i++) line = line OFS $idx[i]
    print line
}
# Does name v match the list? Tries the full value, then the first 2, 3 and 4
# words, so authors are ignored:
#   "Myrcia splendens (Sw.) DC."            -> "Myrcia splendens"
#   "Inga edulis var. parviflora Benth."    -> "Inga edulis var. parviflora"
function hit(v,   w, m) {
    if (v == "") return 0
    if (v in spp) return 1
    m = split(v, w, " ")
    if (m >= 2 && ((w[1] " " w[2]) in spp)) return 1
    if (m >= 3 && ((w[1] " " w[2] " " w[3]) in spp)) return 1
    if (m >= 4 && ((w[1] " " w[2] " " w[3] " " w[4]) in spp)) return 1
    return 0
}
BEGIN {
    FS = OFS = "\t"
    ncols = split(cols, want, " ")
    nkeys = split(keys, keyname, " ")
    t0 = systime()
}

# 1st file: species list -> lookup table (strip Windows \r and stray spaces)
FNR == NR {
    sub(/\r$/, ""); gsub(/^[ \t]+|[ \t]+$/, ""); gsub(/  +/, " ")
    if ($0 != "") { spp[$0] = 1; nspp++ }
    next
}

# header of occurrence.txt -> column positions
FNR == 1 {
    bytes += length($0) + 1
    sub(/\r$/, "")
    for (i = 1; i <= NF; i++) pos[$i] = i
    nk = 0
    for (j = 1; j <= nkeys; j++) {
        if (keyname[j] in pos) kpos[++nk] = pos[keyname[j]]
        else print "WARNING: match column \"" keyname[j] "\" not in header, skipped" > "/dev/stderr"
    }
    if (nk == 0) {
        print "ERROR: none of the match columns exist in occurrence.txt" > "/dev/stderr"
        err = 1; exit 1
    }
    for (j = 1; j <= ncols; j++) {
        if (want[j] in pos) idx[++n] = pos[want[j]]
        else print "WARNING: column \"" want[j] "\" not in header, skipped" > "/dev/stderr"
    }
    printrow(); next
}

# data rows: keep if any match column hits the list
{
    bytes += length($0) + 1
    sub(/\r$/, "")
    total++
    for (j = 1; j <= nk; j++) {
        if (hit($kpos[j])) { printrow(); kept++; break }
    }
    if (total % 100000 == 0) progress()
}

END {
    if (err) exit 1
    progress(); printf "\n" > "/dev/stderr"
    printf "Names in list: %d\n", nspp > "/dev/stderr"
    printf "Rows read: %d | rows kept: %d\n", total, kept > "/dev/stderr"
}
' "$SPECS_FILE" "$OCCURRENCE_FILE" > "$OUTPUT_TXT"

# --------------------------------------------------------------------- zip --
rm -f "$FINAL_OUTPUT"
zip -j "$FINAL_OUTPUT" "$OUTPUT_TXT"
[[ "$KEEP_TXT" == true ]] || rm -f "$OUTPUT_TXT"

echo "Done. Output: $FINAL_OUTPUT"