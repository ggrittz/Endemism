#!/bin/bash

# Before running anything:
# I am currently running the script using MSYS2 UCRT64 as my Unix-like development environment for Windows
# You can still it from https://www.msys2.org/
# Some packages below will require to be previously installed
# To install a package, you can use the following command: pacman -S packagename
# For zip package, for example, just use pacman -S zip
# The script is built in a way that it should be run from inside the analyses folder from the rcompendium structure:
# https://cran.r-project.org/web/packages/rcompendium/index.html
# It doesn't need to be run exactly like that, but paths will need to be changed accordingly

# After saving the script to your computer, first you need to give permission to execute (just need to be done once):
# Go to the analyses path
# 1. cd path/to/analyses

# Use chmod to make it executable
# 2. chmod +x script_name.sh

# Voilà. Now you can run it (but remember to edit variables accordingly)
# 3. bash script_names.sh

# Variables
OCCURRENCE_FILE="reflora_occurrences.txt"
SPECS_FILE="/d/Doutorado/SynTreeSys/Endemism/data/derived-data/spp_names.txt"
FILTERED_OUTPUT="/d/filtered_occurrences.txt"
MERGED_OUTPUT="/d/merged_occurrences.txt"
FINAL_OUTPUT="/d/reflora_cleaned_occurrences.zip"

# Columns to keep
COLUMNS=()

# Step 3: Extract and save the header from occurrence.txt
HEADER=$(head -n 1 "$OCCURRENCE_FILE")
echo "$HEADER" > /d/header.txt

# Step 4: Filter occurrence.txt using spp_names.txt
# Use grep to filter based on species names while retaining original format
grep -F -f "$SPECS_FILE" "$OCCURRENCE_FILE" > "$FILTERED_OUTPUT"

# Step 5: Filter columns based on specified headers
# Extract the column indices for the desired columns
COL_INDICES=$(echo "$HEADER" | awk -v cols="${COLUMNS[*]}" 'BEGIN {
    split(cols, col_array, " "); # Split the desired columns into an array
}
{
    # Loop through all fields (columns) in the header
    for (i = 1; i <= NF; i++) {
        for (j in col_array) {
            if ($i == col_array[j]) {
                # Print the column index (1-based)
                printf "%d,", i
            }
        }
    }
}' | sed 's/,$//')  # Remove trailing comma

# Use awk to keep only the desired columns
echo "$HEADER" | awk -v indices="$COL_INDICES" 'BEGIN { FS=OFS=","; split(indices, idxs, ",") }
NR==1 {
    # Print the header with the selected columns
    for (i = 1; i <= length(idxs); i++) {
        if (i == length(idxs)) {
            print $idxs[i]
        } else {
            printf "%s\t", $idxs[i]
        }
    }
}
NR>1 {
    # Print the data rows with the selected columns
    for (i = 1; i <= length(idxs); i++) {
        if (i == length(idxs)) {
            print $idxs[i]
        } else {
            printf "%s\t", $idxs[i]
        }
    }
}' header.txt "$FILTERED_OUTPUT" > "$MERGED_OUTPUT"

# Step 6: Zip the final output
zip "$FINAL_OUTPUT" "$MERGED_OUTPUT"

# Cleanup
rm "$ZIP_FILE" "$OCCURRENCE_FILE" header.txt "$FILTERED_OUTPUT" "$MERGED_OUTPUT"

echo "Process completed. The final zipped output is: $FINAL_OUTPUT"
