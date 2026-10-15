
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
# Start from an empty folder so no old family files survive a new run
unlink(file_path, recursive = TRUE)
dir.create(file_path)

# Log with the number of registers per family after each step (shared with 07)
# Lines are appended per family, so counts survive a crash or a partial re-run
log_file <- here::here("outputs", "n_registers_log.txt")

# Remove previously cleaned families
# families_done <- list.dirs(here::here("data", "derived-data", "Cleaned_occs"),
#                           full.names = FALSE)
# families_done <- families_done[!grepl("^$", families_done)]
# family_names <- setdiff(family_names, families_done)

# Loop through families
for (i in family_names) {
  cat("Cleaning duplicates for", i, "\n")
  family_data <- vector("list", length(sources))
  
  for (j in seq_along(sources)) {
    family_folders <- list.dirs(sources[j], recursive = FALSE)
    temp_list <- list()
    
    for (k in family_folders) {
      rds_files <- list.files(k, pattern = paste0(i, "\\.rds$"), full.names = TRUE)
      
      for (rds_file in rds_files) {
        data <- readRDS(rds_file)
        
        if ("out.check" %in% colnames(data)) {
          data$out.check <- ifelse(data$out.check == "no.coords", NA, data$out.check)
        }
        
        # Convert all columns to character
        data[] <- lapply(data, as.character)
        temp_list <- append(temp_list, list(data))
      }
    }
    
    if (length(temp_list) > 0) {
      family_data[[j]] <- dplyr::bind_rows(temp_list)
    }
  }
  
  family_data <- family_data[!sapply(family_data, is.null)]
  
  if (length(family_data) > 0) {
    family_data <- dplyr::bind_rows(family_data)
  } else {
    family_data <- NULL
  }
  
  if(is.null(family_data) || nrow(family_data) == 0){
    cat("Skipping", " ", i, ":", " no registers found\n", sep = "")
    dir.create(file.path(file_path, i), showWarnings = FALSE)
    data.table::fwrite(data.frame(family = i,
                                  step = c("before_dup", "after_dup"),
                                  n_registers = 0),
                       log_file, append = TRUE, sep = "\t")
    next
  }
  
  ##### Save new getDup error HERE! #####
  # saveRDS(family_data, here::here("getDup_error_new.rds"))
  
  # Remove irrelevant additional columns used before
  #to_remove <- c("datelastmodified", "scientificname_full",
  #               "scientificName.1", "gbifID")
  #family_data <- family_data[, -which(colnames(family_data) %in% to_remove)]
  
  # Count registers per family before deduplication
  n_registers_before <- nrow(family_data)
  
  # Start processing the duplicates -----------------------------------------
  
  # Creating unique record identifiers
  family_data$numTombo <- plantR::getTombo(collection = family_data[, c("collectionCode.new")],
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
  
  # Append this family's counts to the log
  data.table::fwrite(data.frame(family = i,
                                step = c("before_dup", "after_dup"),
                                n_registers = c(n_registers_before, nrow(family_data))),
                     log_file, append = TRUE, sep = "\t")
  
  # id for later usage
  family_data$ids <- paste0(i, "_", 1:nrow(family_data))
  
  # Saving the cleaned data
  # Saving by collection and separating by family
  plantR::saveData(family_data,
                   dir.name = i,
                   path = file_path,
                   by = "family",
                   file.format = "rds",
                   compress = TRUE)
  
  # Save as .gz.csv
  # data.table::fwrite(family_data, file.path(file_path, i, paste0(i, ".csv.gz")))
  # Save as .parquet
  # arrow::write_parquet(family_data, file.path(file_path, i, paste0(i, ".parquet")))
}

# Reading the log later (keeps the most recent line when a family was re-run)
# n_log <- data.table::fread(here::here("outputs", "n_registers_log.txt"))
# n_log <- n_log[!duplicated(n_log[, c("family", "step")], fromLast = TRUE), ]
