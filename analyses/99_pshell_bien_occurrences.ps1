# Downloading BIEN data from its server
# Database connection details
$SERVER = "vegbiendev.nceas.ucsb.edu"
$DBNAME = "public_vegbien"
$USER = "public_bien"
$PASSWORD = "bien_public"

# Output file path
$OUTPUT_DIR = "D:\"
$OUTPUT_FILE = "$OUTPUT_DIR\bien_ocurrences.txt"  # Save as .txt
$ZIP_FILE = "$OUTPUT_DIR\bien_occurrences.zip"    # Zip file path

# SQL query to retrieve specific columns for higher plant groups excluding bryophytes
# and filtering observation_type to include 'plot', 'literature', and 'checklist' (excluding 'specimen')
# Replace null values with 'NA' using COALESCE
$QUERY = @"
SELECT 
    COALESCE(datasource_id::text, 'NA') AS datasource_id, 
    COALESCE(custodial_institution_codes, 'NA') AS custodial_institution_codes, 
    COALESCE(collection_code, 'NA') AS collection_code, 
    COALESCE(dataset, 'NA') AS dataset, 
    COALESCE(observation_type, 'NA') AS observation_type, 
    COALESCE(catalog_number, 'NA') AS catalog_number, 
    COALESCE(record_number, 'NA') AS record_number, 
    COALESCE(recorded_by, 'NA') AS recorded_by, 
    COALESCE(date_collected::text, 'NA') AS date_collected, 
    COALESCE(country, 'NA') AS country, 
    COALESCE(state_province, 'NA') AS state_province, 
    COALESCE(county, 'NA') AS county, 
    COALESCE(locality, 'NA') AS locality, 
    COALESCE(latitude::text, 'NA') AS latitude, 
    COALESCE(longitude::text, 'NA') AS longitude, 
    COALESCE(identified_by, 'NA') AS identified_by, 
    COALESCE(date_identified::text, 'NA') AS date_identified, 
    COALESCE(identification_remarks, 'NA') AS identification_remarks, 
    COALESCE(name_matched, 'NA') AS name_matched, 
    COALESCE(family_matched, 'NA') AS family_matched, 
    COALESCE(scrubbed_genus, 'NA') AS scrubbed_genus, 
    COALESCE(name_matched_author, 'NA') AS name_matched_author, 
    COALESCE(scrubbed_taxonomic_status, 'NA') AS scrubbed_taxonomic_status,
    COALESCE(higher_plant_group, 'NA') AS higher_plant_group
FROM 
    view_full_occurrence_individual 
WHERE 
    higher_plant_group IN ('ferns and allies', 'flowering plants', 'gymnosperms (conifers)', 'gymnosperms (non-conifer)')
    AND observation_type IN ('plot', 'literature', 'checklist');
"@

# Set the PGPASSWORD environment variable
$env:PGPASSWORD = $PASSWORD

# Step 1: Get the total number of rows to process
$COUNT_QUERY = @"
SELECT COUNT(*) 
FROM view_full_occurrence_individual 
WHERE 
    higher_plant_group IN ('ferns and allies', 'flowering plants', 'gymnosperms (conifers)', 'gymnosperms (non-conifer)')
    AND observation_type IN ('plot', 'literature', 'checklist');
"@

# Execute the count query
$TOTAL_ROWS = (psql -h $SERVER -U $USER -d $DBNAME -c "$COUNT_QUERY" -t -A)
Write-Output "Total rows to process: $TOTAL_ROWS"

# Step 2: Execute the main query and track progress
$PSQL_COMMAND = "psql -h $SERVER -U $USER -d $DBNAME -c `"$QUERY`" -A -F `"`t`""

# Initialize progress tracking
$ROW_COUNT = 0
$PERCENT_COMPLETE = 0

# Execute the query and process the output line by line
Invoke-Expression $PSQL_COMMAND | ForEach-Object {
    # Write the current row to the output file
    $_ | Out-File -FilePath $OUTPUT_FILE -Encoding UTF8 -Append

    # Update progress
    $ROW_COUNT++
    $PERCENT_COMPLETE = [math]::Round(($ROW_COUNT / $TOTAL_ROWS) * 100, 2)
    Write-Progress -Activity "Downloading Data" -Status "$PERCENT_COMPLETE% Complete" -PercentComplete $PERCENT_COMPLETE
}

# Check if the query was successful
if ($LASTEXITCODE -eq 0) {
    Write-Output "Data successfully saved to $OUTPUT_FILE"
} else {
    Write-Output "Error: Failed to execute the query."
}

# Step 3: Compress the .txt file into a .zip archive
if (Test-Path $OUTPUT_FILE) {
    Write-Output "Compressing $OUTPUT_FILE into $ZIP_FILE..."
    Compress-Archive -Path $OUTPUT_FILE -DestinationPath $ZIP_FILE -Force
    Write-Output "Compression complete. Zip file saved to $ZIP_FILE"
} else {
    Write-Output "Error: $OUTPUT_FILE does not exist. Compression skipped."
}

# Clear the PGPASSWORD environment variable for security
$env:PGPASSWORD = $null