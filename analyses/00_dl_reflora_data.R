# Download REFLORA data ---------------------------------------------------

# https://ipt.jbrj.gov.br/reflora/resource?r= is the base url.

# Load the list of herbaria available in REFLORA 
file_path <- here::here("data", "derived-data")
file_name <- "reflora_herbarium_list.txt" # EDIT: checked on 25/09/26
herbaria <- data.table::fread(file.path(file_path, file_name))
herbaria <- as.data.frame(herbaria)
herbaria <- herbaria[order(herbaria$herbarium), ]
# Path to save
path_to_save <- here::here("data", "raw-data", "REFLORA")
if(!dir.exists(path_to_save))  # EDIT
  dir.create(path_to_save)     # EDIT
# Our scientific names to filter later
our_names <- here::here("data", "derived-data", "checklist_species_level.rds")
our_names <- readRDS(our_names)


# From REFLORA IPT, get each herbarium and the most updated version of the data

for(i in seq_along(herbaria)){
  
  cat("Downloading", herbaria[i], "\n")
  
  # Get REFLORA IPT url
  url <- paste0("https://ipt.jbrj.gov.br/reflora/resource?r=", herbaria[i])
  
  tryCatch({
    
    # Get its html content
    html_content <- rvest::read_html(url, encoding = "ISO-8859-1")
    
    # Find the node containing the most updated version (SelectorGadget addon is useful here)
    node <- rvest::html_node(html_content, css = "td a")
    
    # Get href attribute
    link <- rvest::html_attr(node, "href")
    
    # Download and save
    name_to_save <- paste0(herbaria[i], ".zip")
    download.file(url = link, destfile = file.path(path_to_save, name_to_save), mode = "wb")
    
  }, error = function(e) cat("  FAILED:", herbaria[i], "-", conditionMessage(e), "\n"))  # EDIT
  
}

herbaria_use <- herbaria[herbaria != "acam"]

# Extract each zip file before binding it all
for(h in herbaria_use){
  zip_file <- file.path(path_to_save, paste0(h, ".zip"))
  if(file.exists(zip_file))
    utils::unzip(zip_file, exdir = file.path(path_to_save, h))
}

# EDIT: build the paths from the herbarium codes instead of list.files(),
# so zip files, old folders or sort order can't shift anything
occ_files <- file.path(path_to_save, herbaria_use, "occurrence.txt")
has_data <- file.exists(occ_files)
cat("Herbaria without occurrence data:", herbaria_use[!has_data], "\n")

# Load each occurrence.txt from each collection and bind it
herbarios <- lapply(seq_along(occ_files[has_data]), function(i){
  cat("Reading herbaria", herbaria_use[has_data][i], "\n")
  data.table::fread(file = occ_files[has_data][i],
                    sep = "\t", quote = "",
                    encoding = "UTF-8",
                    colClasses = "character")
})
names(herbarios) <- herbaria_use[has_data]

# Remove empty data frames
herbarios <- herbarios[sapply(herbarios, function(x) x[, .N] > 0)]

# dplyr::bind_rows also fills missing columns with NA
all_data <- dplyr::bind_rows(herbarios, .id = 'my_id')  # my_id now holds the herbarium code
all_data[all_data == ""] <- NA

# Some initial filters
# Remove NA and empty names from scientificName column
all_data <- all_data[!is.na(all_data$scientificName), ]
# Keep only taxon ranks at species, variety, subsp, form levels
all_data <- all_data[all_data$taxonRank %in% c("ESPECIE", "f.", "FORMA", 
                                               "Infr.", "specie", "ssp.",
                                               "SUB_ESPECIE", "var.",
                                               "VARIEDADE"), ]

# Keep only thinds ending in ACEAE or aceae in family column (lots of noise here)
all_data <- all_data[grepl("ACEAE$|aceae$", all_data$family), ]
# Make family names Upper-lowercase
all_data$family <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", all_data$family, perl = TRUE)
# Make genus names Upper-lowercase
all_data$genus <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", all_data$genus, perl = TRUE)
# Guarantee that epithet and specific epithet are lowercased
all_data$specificEpithet <-tolower(all_data$specificEpithet)
all_data$infraspecificEpithet <- tolower(all_data$infraspecificEpithet)

# Remake scientificName column by keeping only the canonical
all_data$scientificName <- NA

# Make VARIETIES
make_these <- which(grepl("var\\.|VARIEDADE", all_data$taxonRank))
all_data$scientificName[make_these] <- paste(all_data$genus[make_these],
                                             all_data$specificEpithet[make_these],
                                             "var.",
                                             all_data$infraspecificEpithet[make_these])

# Make SUBSPECIES
make_these <- which(grepl("SUB_ESPECIE|ssp\\.", all_data$taxonRank))
all_data$scientificName[make_these] <- paste(all_data$genus[make_these],
                                             all_data$specificEpithet[make_these],
                                             "subsp.",
                                             all_data$infraspecificEpithet[make_these])

# Make FORMA
make_these <- which(grepl("FORMA|f\\.", all_data$taxonRank))
all_data$scientificName[make_these] <- paste(all_data$genus[make_these],
                                             all_data$specificEpithet[make_these],
                                             "f.",
                                             all_data$infraspecificEpithet[make_these])

# MAKE SPECIES
make_these <- which(grepl("^ESPECIE$|specie", all_data$taxonRank))
all_data$scientificName[make_these] <- paste(all_data$genus[make_these],
                                             all_data$specificEpithet[make_these])
# Can't deal with "Infr."
all_data <- all_data[!grepl("Infr\\.", all_data$taxonRank), ]

# Make full name
all_data$scientificNameFull <- plantR:::buildName(all_data,
                                                  col.names = c("scientificName",
                                                                "scientificNameAuthorship"))
# Keep only names found in our list of names
all_data1 <- all_data[all_data$scientificNameFull %in% 
                        our_names$input_full_name, ]
rm(all_data); gc()

# Get taxonomic info from our list of name
# Match and substitute the reflora name by our checklist name, already manually checked
match_indices <- match(all_data1$scientificNameFull, our_names$input_full_name)
all_data1$scientificName <- our_names$our_accepted_scientificName[match_indices]
all_data1$family <- our_names$our_accepted_taxonFamily[match_indices]
all_data1$taxonAuthor <- our_names$our_accepted_taxonAuthorship[match_indices]
all_data1$taxonName <- our_names$our_accepted_taxonName[match_indices]

# # Make scientificNameStatus
all_data1$scientificNameStatus <- plantR::fixSpecies(x = all_data1$scientificNameFull)$scientificNameStatus
all_data1 <- all_data1[, -c("scientificNameFull")] # to avoid confusion

# Create minimum necessary columns for plantR::formatDwc
all_data1$county <- NA

# Names that became NA are species that are not in our checklist, i.e., 
# species from outside of the Neotropics; so remove
all_data1 <- all_data1[!is.na(all_data1$scientificName), ]

# Save
data.table::fwrite(all_data1, file = "C:/Users/ggrittz/Desktop/data/reflora_data/reflora_occurrence_filtered_new.txt")
