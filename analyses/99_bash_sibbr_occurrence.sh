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

OCCURRENCE_FILE="/d/sibbr_dl_occurrences.txt"
SPECS_FILE="/d/Doutorado/SynTreeSys/Endemism/data/derived-data/spp_names.txt"
FILTERED_OUTPUT="sibbr_cleaned_occ.txt"
FINAL_OUTPUT="/d/sibbr_occurrences.zip"

# Step 1: Extract and save the header from the .txt file
HEADER=$(head -n 1 "$OCCURRENCE_FILE")

# Step 2: Filter occurrence.txt using spp_names.txt and add the header back
echo "$HEADER" > "$FILTERED_OUTPUT"  # Write the header to the file
grep -F -f "$SPECS_FILE" "$OCCURRENCE_FILE" >> "$FILTERED_OUTPUT"  # Append the filtered content

# Step 3: Zip the final output
zip "$FINAL_OUTPUT" "$FILTERED_OUTPUT"

# Cleanup
rm "$OCCURRENCE_FILE" "$FILTERED_OUTPUT"

echo "Process completed. The final zipped output is: $FINAL_OUTPUT"