
# Create the derived-data folders for speciesLink data
file_path <- here::here("data", "derived-data", "speciesLink_occ")
if(!dir.exists(file_path))
  dir.create(file_path)

# Loading all family names presented in our checklist
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))

# Loading our taxonomy to fix the names
our_names <- readRDS(file.path(here::here("data", "derived-data", "checklist_species_level.rds")))

# Remove families already prepared, if any
# families_done <- list.dirs(here::here("data", "derived-data", "speciesLink_occ"), 
#                            full.names = FALSE)
# families_done <- families_done[!grepl("^$", families_done)]
# family_names <- setdiff(family_names, families_done)

# Pre-loading objects that will be needed in the pipeline below
islandsBuff <- plantR::islandsBuff
landBuff <- plantR::landBuff
latamMap <- plantR::latamMap
shoreLines <- plantR::shoreLines
world <- plantR::world
worldMap <- plantR::worldMap

##### Fix plantR's collection codes table (run once, before the loop) #####
# plantR:::collectionCodes has duplicated keys (e.g. "POL_POL"), which makes
# getCode() duplicate rows in its merge -> row-count mismatch in cbind
cc <- plantR:::collectionCodes
cc <- cc[order(!is.na(cc$col.OBS)), ]            # prefer proper IH entries
cc <- cc[!duplicated(cc$collection.string), ]
utils::assignInNamespace("collectionCodes", cc, ns = "plantR")
rm(cc)

# Country and state names from the plantR gazetteer written differently in the
# WGSRPD table used by plantR::checkDist (reported to plantR); used only to build
# a temporary location column for checkDist below
loc_alias <- read.csv(here::here("data", "derived-data", "plantR_loc_alias.csv"))
loc_alias <- setNames(loc_alias$taxon.distribution.bru, loc_alias$loc)

# Loading internal functions
devtools::load_all()

# ##### ALREADY PERFORMED #####
# # Necessary step due to registers without author name in speciesLink data
# library(plantRdata)
# data(wfoNames) # is this the right way to call it?
# file_path2 <- here::here("data", "raw-data", "speciesLink", "speciesLink_occurrences.txt")
# splink_data <- data.table::fread(file_path2)
# splink_data <- as.data.frame(splink_data)
# # ID for later use
# splink_data$myID <- 1:dim(splink_data)[1]
# # Remove everything that does not have, at least, a species name
# # i.e, things on genus- or family-level and other weirds things from speciesLink
# splink_data <- plantR::fixSpecies(splink_data,
#                                  tax.name = "scientificname",
#                                  author.name = "scientificnameauthor")
# splink_data <- splink_data[!splink_data$scientificNameStatus %in% "indet", ]
# 
# # name matching with cases that do not contain a author name
# splink_data <- plantR::prepSpecies(splink_data,
#                                    tax.names = c("scientificName.new",
#                                                  "scientificNameAuthorship.new"),
#                                    db = wfoNames,
#                                    use.authors = TRUE,
#                                    replace.names = FALSE,
#                                    # clean.names = TRUE,
#                                    # parallel = TRUE,
#                                    split.letters = TRUE,
#                                    # cores = 12,
#                                    show.progress = TRUE)
# 
# # Save
# data.table::fwrite(splink_data, 
#                    file = here::here("data", "raw-data", "speciesLink", "speciesLink_occurrence_filtered.txt"))
##### ALREADY PERFORMED ABOVE #####


# Preparing speciesLink data ----------------------------------------------

# speciesLink data (read once; filtered by family inside the loop)
file_path2 <- here::here("data", "raw-data", "speciesLink", "speciesLink_occurrence_filtered.txt")
splink_all <- data.table::fread(file_path2)

# Loop over our accepted families: the family written in speciesLink is messy
# (authors, typos, "/" and ":" that cannot be folder names on Windows), so each
# record gets the accepted family of its name before the loop; records whose
# name is not in our checklist are dropped here instead of inside the loop
splink_all$family <- our_names$our_accepted_taxonFamily[match(splink_all$scientificNameFull,
                                                               our_names$input_full_name)]
splink_all <- splink_all[!is.na(family), ]
splink_families <- sort(unique(splink_all$family))

for(i in splink_families){

  cat("Preparing", i, "\n")

  # Filter by family
  splink_data <- splink_all[family == i, ]
  
  # If data loaded is NULL or has no rows
  if (is.null(splink_data) || nrow(splink_data) == 0) {
    cat("Skipping", " ", i, ":", " no registers found\n", sep = "")
    dir.create(file.path(file_path, i), showWarnings = FALSE)
    next
  }
  
  # Convert the data.table to data.frame for plantR
  splink_data <- as.data.frame(splink_data)
  
  # Keep only rows that contain a scientificName found in our taxonomy input_full_name
  splink_data <- splink_data[splink_data$scientificNameFull %in% 
                               our_names$input_full_name, ]
  
  if (is.null(splink_data) || nrow(splink_data) == 0) {
    cat("Skipping", " ", i, ":", " no registers found\n", sep = "")
    dir.create(file.path(file_path, i), showWarnings = FALSE)
    next
  }
  
  # Match and substitute the gbif name by our checklist name, already manually checked
  match_indices <- match(splink_data$scientificNameFull, our_names$input_full_name)
  splink_data$scientificName <- our_names$our_accepted_scientificName[match_indices]
  splink_data$family <- our_names$our_accepted_taxonFamily[match_indices]
  splink_data$taxonAuthor <- our_names$our_accepted_taxonAuthorship[match_indices]
  splink_data$taxonName <- our_names$our_accepted_taxonName[match_indices]
  
  # Names that became NA are species that are not in our checklist, i.e., species from outside
  # of the Neotropics; so remove
  splink_data <- splink_data[!is.na(splink_data$scientificName), ]
  
  if (is.null(splink_data) || nrow(splink_data) == 0) {
    cat("Skipping", " ", i, ":", " no registers found\n", sep = "")
    dir.create(file.path(file_path, i), showWarnings = FALSE)
    next
  }
  
  # Remove previous columns and make them again below
  cols <- c("scientificName.new", "scientificNameAuthorship.new", "scientificNameStatus",
            "suggestedFamily", "suggestedName", "suggestedAuthorship", 
            "taxon.rank", "tax.notes", "id", "scientificNameFull",
            "scientificname", "scientificnameauthor")
  splink_data <- splink_data[, !names(splink_data) %in% cols]
  # Adding the scientificNameStatus column from fixSpecies here
  splink_data$scientificNameStatus <- plantR::fixSpecies(x = splink_data,
                                                         tax.name = "taxonName",
                                                         author.name = "taxonAuthor")$scientificNameStatus
  
  # Editing some columns since splink changed their column names...
  names(splink_data)[which(names(splink_data) == "collector")] <- "recordedby"
  names(splink_data)[which(names(splink_data) == "collectornumber")] <- "recordnumber"
  names(splink_data)[which(names(splink_data) == "scientificName")] <- "scientificname"
  splink_data$scientificnameauthorship <- splink_data$taxonAuthor

  # Data formatting in DwC
  splink_data <- plantR::formatDwc(splink_data = splink_data)

  
  # Data standardization ----------------------------------------------------
  
  
  ##### Standardizing collection codes #####
  splink_data <- plantR::getCode(splink_data) # later update plantR:::collectionCodes with 00_get_collections.R
  
  ##### Standardizing names of collectors and determiners #####
  
  # First collector
  splink_data$recordedBy.new <- plantR::prepName(splink_data$recordedBy,
                                                 output = "first",
                                                 sep.out = "; ")
  # Auxilliary collectors
  splink_data$recordedBy.aux <- plantR::prepName(splink_data$recordedBy,
                                                 output = "aux",
                                                 sep.out = "; ")
  # First determiner
  splink_data$identifiedBy.new <- plantR::prepName(splink_data$identifiedBy,
                                                   output = "first",
                                                   sep.out = "; ")
  # Auxilliary determiners
  splink_data$identifiedBy.aux <- plantR::prepName(splink_data$identifiedBy,
                                                   output = "aux",
                                                   sep.out = "; ")
  
  # Name notations
  splink_data$recordedBy.new <- plantR::missName(splink_data$recordedBy.new,
                                               type = "collector",
                                               noName = "s.n")
  splink_data$identifiedBy.new <- plantR::missName(splink_data$identifiedBy.new,
                                                 type = "determiner",
                                                 noName = "s.n")
  
  # Extract the last name of the collector to future standardization
  splink_data$last.name <- plantR::lastName(splink_data$recordedBy.new)
  
  # Collector number
  splink_data$recordNumber.new <- plantR::colNumber(splink_data$recordNumber, noNumb = "s.n.")
  
  # Collection and identification year #### Check with Renato: plantR::getYear("01/02/99") ####
  # if we use as.Date("01/02/99", format = "%d/%m/%y") we get 1999 but this may well be 1899...
  
  splink_data$year.new <- plantR::getYear(splink_data$year, noYear = "n.d.")
  splink_data$yearIdentified.new <- plantR::getYear(splink_data$dateIdentified, noYear = "n.d.")
  
  
  # Data editing ------------------------------------------------------------
  
  
  # Formatting locality information at different scales
  splink_data <- plantR::fixLoc(splink_data,
                              loc.levels = c("country",
                                             "stateProvince", 
                                             "municipality", 
                                             "locality"),
                              scrap = TRUE)
  
  # Get and simplify locality string create by strLoc
  locs <- plantR::strLoc(splink_data)
  
  # Simplify
  locs$loc.string <- plantR::prepLoc(locs$loc.string) # priority string
  locs$loc.string1 <- plantR::prepLoc(locs$loc.string1) # alternative string 1
  locs$loc.string2 <- plantR::prepLoc(locs$loc.string2) # alternative string 2
  
  # Cross the simplified strings against plantR's gazetteer
  locs <- plantR::getLoc(locs, gazet = "plantR")
  
  # Bind the information obtained from the gazetteer into splink_data
  splink_data <- cbind.data.frame(splink_data, locs[, c("loc",
                                                    "loc.correct",
                                                    "latitude.gazetteer",
                                                    "longitude.gazetteer",
                                                    "resolution.gazetteer")])
  
  
  ##### Editing geographical coordinates #####
  
  # Transforms the coordinates into decimal degrees
  splink_data <- plantR::prepCoord(splink_data)
  
  # Missing coordinates can be obtained from the gazetteer
  splink_data <- plantR::getCoord(splink_data)
  
  
  # Skipping formatTax steps since we already used our taxonomy to fix the names
  
  
  # Data validation ---------------------------------------------------------
  
  
  # Validating locality information
  splink_data <- plantR::validateLoc(splink_data)
  
  # Validating original coordinates at country, state, and county level
  splink_data <- plantR::checkCoord(splink_data,
                                    keep.cols = c("geo.check",
                                                  "NAME_0",
                                                  "country.gazet"))
  
  # Validating coordinates due to rounding precision
  splink_data <- plantR::checkBorders(splink_data, output = "same.col")
  
  # Validating coordinates in the sea but close to the shore
  splink_data <- plantR::checkShore(splink_data, output = "same.col")
  
  # Validating inverted and swapped coordinates
  splink_data <- plantR::checkInverted(splink_data, output = "same.col")
  
  # Validating records from cultivated individuals
  splink_data <- plantR::getCult(splink_data)
  
  # Validating spatial outliers
  splink_data <-  plantR::checkOut(splink_data, 
                                   tax.name = "scientificName",
                                   clas.cut = 2.5,
                                   rob.cut = 12,
                                   keep.values = TRUE)

  # Validating records that are falling outside of the species' indicated botanical country
  # Temporary location column with country/state names as written in WGSRPD
  loc_state <- sub("^([^_]*_[^_]*).*", "\\1", splink_data$loc.correct)
  fix_these <- grepl("_", loc_state) & loc_state %in% names(loc_alias)
  splink_data$loc.dist <- splink_data$loc.correct
  splink_data$loc.dist[fix_these] <- paste0(sub("_.*", "_", loc_state[fix_these]),
                                      loc_alias[loc_state[fix_these]],
                                      sub("^[^_]*_[^_]*", "", splink_data$loc.correct[fix_these]))
  loc_country <- sub("_.*", "", splink_data$loc.dist)
  fix_these <- loc_country %in% names(loc_alias)
  splink_data$loc.dist[fix_these] <- paste0(loc_alias[loc_country[fix_these]],
                                      sub("^[^_]*", "", splink_data$loc.dist[fix_these]))
  splink_data <- plantR::checkDist(x = splink_data,
                              tax.name = "taxonName",
                              tax.author = "taxonAuthor",
                              loc = "loc.dist",
                              source = "wcvp")
  splink_data$loc.dist <- NULL
  
  # Validating confidence level of species determinators
  splink_data <- plantR::validateTax(splink_data, 
                                     kingdom = "plantae",
                                     generalist = FALSE, print = FALSE)
  
  # Considering low complexity ID as "high"
  ###### Gotta add this in the next version #####
  
  # Saving by collection and separating by family
  plantR::saveData(splink_data,
                   dir.name = i,
                   path = file_path,
                   by = "family",
                   file.format = "rds",
                   compress = TRUE)
}
