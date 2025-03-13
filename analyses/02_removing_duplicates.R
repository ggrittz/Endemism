
# Removing duplicates -----------------------------------------------------
devtools::load_all()

# Loading family names
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))

# List folders that will be searched for families
search_path <- here::here("data", "derived-data")
sources <- list.files(search_path, pattern = "_occ$", full.names = TRUE)

# Create the folder to save all cleaned
file_path <- here::here("data", "derived-data", "Cleaned_occs")
if(!dir.exists(file_path))
  dir.create(file_path)

# Remove previously cleaned families
families_done <- list.dirs(here::here("data", "derived-data", "Cleaned_occs"), 
                           full.names = FALSE)
families_done <- families_done[!grepl("^$", families_done)]
family_names <- setdiff(family_names, families_done)

# Loop through families
for (i in family_names) {
  cat("Cleaning duplicates for", i, "\n")
  family_data <- vector("list", length(sources))
  
  # Loop through sources
  for (j in seq_along(sources)) {
    
    # List all family folders inside each source folder
    family_folders <- list.dirs(sources[j], recursive = FALSE)
    
    # Loop through all family folders to find .rds files
    for (k in family_folders) {
      # Check if the folder contains an .rds file matching the family name
      rds_files <- list.files(k, pattern = paste0(i, "\\.rds$"), full.names = TRUE)
      
      # If any matching .rds file is found, read and store it
      if (length(rds_files) > 0) {
        for (rds_file in rds_files) {
          # Read the .rds file
          data <- readRDS(rds_file)
          
          # Check for "no.coords" and convert to NA
          if ("out.check" %in% colnames(data)) {
            data$out.check <- ifelse(data$out.check == "no.coords", NA, data$out.check)
          }
          
          # Store the data for this family from this source
          family_data[[j]] <- data
        }
      }
    }
  }
  
  family_data <- dplyr::bind_rows(family_data)
  
  if(nrow(family_data) == 0){
    cat("Skipping", " ", i, ":", " no registers found\n", sep = "")
    dir.create(file.path(file_path, i), showWarnings = FALSE)
    next
  }
  
  # Remove irrelevant additional columns used before
  to_remove <- c("datelastmodified", "scientificname_full",
                 "scientificName.1", "gbifID")
  family_data <- family_data[, -which(colnames(family_data) %in% to_remove)]
  
  # Start processing the duplicates -----------------------------------------
  
  # Creating unique record identifiers
  family_data$numTombo <- my_getTombo(collection = family_data[, c("collectionCode.new")],
                                      accession = family_data[, c("catalogNumber")])
  
  # Creating strings with diff. sets of information to search for duplicates
  dups <- plantR::prepDup(family_data,
                          col.names = c(family = "family",
                                        species = "scientificName",
                                        col.last.name = "last.name",
                                        col.number = "recordNumber.new",
                                        col.loc = "municipality.new",
                                        col.year = "year.new"),
                          comb.fields = list(c("family", "col.last.name", "col.number", "col.loc"), 
                                             c("family", "col.year", "col.number", "col.loc"), 
                                             c("species", "col.last.name", "col.number", "col.year"), 
                                             c("col.year", "col.last.name", "col.number", "col.loc")))
  
  # Finding duplicates from the strings
  dups <- plantR::getDup(dups)
  
  # Bind this info to our data set
  family_data <- cbind.data.frame(family_data,
                                  dups[, c("dup.ID", "dup.numb", "dup.prop")],
                                  stringsAsFactors = FALSE)
  
  # Homogenizing the information within duplicates
  # scientificNameStatus column not needed here since we are using the taxonomy
  family_data <- plantR::mergeDup(dups = family_data,
                                  tax.names = c(family = "family",
                                                species = "scientificName",
                                                det.name = "identifiedBy.new",
                                                det.year = "yearIdentified.new",
                                                tax.check = "tax.check",
                                                status = "scientificNameStatus"))
  
  # Finally, removing duplicates
  family_data <- plantR::rmDup(df = family_data,
                               rm.all = TRUE)
  
  # Saving the cleaned data
  # Saving by collection and separating by family
  plantR::saveData(family_data,
                   dir.name = i,
                   path = file_path,
                   by = "family",
                   file.format = "rds",
                   compress = TRUE)

}
