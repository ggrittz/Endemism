
# Download BIEN data ------------------------------------------------------

# Our list of families
file_path <- here::here("data", "derived-data")
file_name <- "checklist_standardized.rds"
our_families <- readRDS(file.path(file_path, file_name))

# Unique families to download via BIEN
our_families <- sort(unique(our_families$our_accepted_taxonFamily))

# Define save path
# path_to_save <- "D:/BIEN"
path_to_save <- here::here("data", "raw-data", "BIEN")

# Set up parallel backend
future::plan(multisession, workers = parallel::detectCores() - 1)

# Function to download and save BIEN data
download_family <- function(i) {
  cat("Downloading", i, "\n")
  
  dl_family <- BIEN::BIEN_occurrence_family(family = i,
                                            cultivated = TRUE,
                                            new.world = TRUE,
                                            observation.type = TRUE,
                                            all.taxonomy = TRUE,
                                            native.status = TRUE,
                                            natives.only = FALSE,
                                            political.boundaries = TRUE,
                                            collection.info = TRUE,
                                            only.geovalid = FALSE)
  
  saveRDS(object = dl_family, file = file.path(path_to_save, paste0(i, ".rds")))
}

# Run the function in parallel
future.apply::future_lapply(our_families, download_family)

# Clean up
rm(list = ls())
