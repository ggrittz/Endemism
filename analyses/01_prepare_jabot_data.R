
# Loading internal functions
devtools::load_all()

# Create the derived-data folders for JABOT data
file_path <- here::here("data", "derived-data", "JABOT_occ")
if(!dir.exists(file_path))
  dir.create(file_path)

# Loading all family names presented in our checklist
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))
family_names <- toupper(family_names)

# Loading our taxonomy to fix the names
our_names <- readRDS(file.path(here::here("data", "derived-data", "checklist_standardized.rds")))

# Pre-loading objects that will be needed in the pipeline below
islandsBuff <- plantR::islandsBuff
landBuff <- plantR::landBuff
latamMap <- plantR::latamMap
shoreLines <- plantR::shoreLines
world <- plantR::world
worldMap <- plantR::worldMap


# ##### Already performed #####
# # Loading data
# file_path2 <- "D:/jabot_occurrences.txt"
# jabot_data <- as.data.frame(data.table::fread(file_path2))
# 
# ##### Already performed #####
# # Filter only species
# jabot_data <- jabot_data[!is.na(jabot_data$specificEpithet), ]
# # Separate canonical and author names
# jabot_data1 <- plantR::fixAuthors(jabot_data$scientificName)
# # Substitute back
# jabot_data$scientificName <- jabot_data1$tax.name
# jabot_data$scientificNameAuthorship <- jabot_data1$tax.author
# # Make family names Upper-lowercase (all caps in JABOT)
# jabot_data$family <- gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", jabot_data$family, perl = TRUE)
# # Make full name
# jabot_data$scientificNameFull <- plantR:::buildName(jabot_data,
#                                                     col.names = c("scientificName",
#                                                                   "scientificNameAuthorship"))
# # Keep only names found in our list of names
# jabot_data <- jabot_data[jabot_data$scientificNameFull %in% 
#                            our_names$input_full_name, ]
# 
# # Get taxonomic info from our list of name
# # Match and substitute the jabot name by our checklist name, already manually checked
# match_indices <- match(jabot_data$scientificNameFull, our_names$input_full_name)
# jabot_data$scientificName <- our_names$our_accepted_scientificName[match_indices]
# jabot_data$family <- our_names$our_accepted_taxonFamily[match_indices]
# jabot_data$taxonAuthor <- our_names$our_accepted_taxonAuthorship[match_indices]
# jabot_data$taxonName <- our_names$our_accepted_taxonName[match_indices]
# 
# # Make scientificNameStatus
# jabot_data$scientificNameStatus <- plantR::fixSpecies(x = jabot_data,
#                                                       tax.name = "taxonName",
#                                                       author.name = "taxonAuthor")$scientificNameStatus
# jabot_data <- jabot_data[!grepl("^indet$", jabot_data$scientificNameStatus), ]
# 
# # Create minimum necessary columns for plantR::formatDwc
# jabot_data$county <- NA
# 
# # Names that became NA are species that are not in our checklist, i.e., species from outside
# # of the Neotropics; so remove
# jabot_data <- jabot_data[!is.na(jabot_data$scientificName), ]
# 
# # Save
# data.table::fwrite(jabot_data, file = "D:/jabot_occurrence_filtered_new.txt")

# New start from here
# Loading data (already prepared)
file_path2 <- "D:/jabot_occurrence_filtered_new.txt"
jabot_data <- as.data.frame(data.table::fread(file_path2))


# Preparing JABOT data ----------------------------------------------------

for(i in family_names){
  
  cat("Preparing", i, "\n")
  
    # If data loaded is NULL or has no rows
  if (is.null(jabot_data) || nrow(jabot_data) == 0) {
    cat("Skipping", " ", i, ":", " no registers found\n", sep = "")
    dir.create(file.path(file_path, i), showWarnings = FALSE)
    next
  }
  
  jabot_data <- jabot_data[jabot_data$family %in% i, ]
  
  # Data formatting in DwC
  jabot_data <- plantR::formatDwc(user_data = jabot_data)
  
  
  # Data standardization ----------------------------------------------------
  
  
  ##### Standardizing collection codes #####
  jabot_data <- plantR::getCode(jabot_data) # later update plantR:::collectionCodes with 00_get_collections.R
  
  ##### Standardizing names of collectors and determiners #####
  
  # First collector
  jabot_data$recordedBy.new <- plantR::prepName(jabot_data$recordedBy,
                                            output = "first",
                                            sep.out = "; ")
  # Auxilliary collectors
  jabot_data$recordedBy.aux <- plantR::prepName(jabot_data$recordedBy,
                                            output = "aux",
                                            sep.out = "; ")
  # First determiner
  jabot_data$identifiedBy.new <- plantR::prepName(jabot_data$identifiedBy,
                                              output = "first",
                                              sep.out = "; ")
  # Auxilliary determiners
  jabot_data$identifiedBy.aux <- plantR::prepName(jabot_data$identifiedBy,
                                              output = "aux",
                                              sep.out = "; ")
  
  # Name notations
  jabot_data$recordedBy.new <- plantR::missName(jabot_data$recordedBy.new,
                                                 type = "collector",
                                                 noName = "s.n")
  jabot_data$identifiedBy.new <- plantR::missName(jabot_data$identifiedBy.new,
                                                   type = "determiner",
                                                   noName = "s.n")
  
  # Extract the last name of the collector to future standardization
  jabot_data$last.name <- plantR::lastName(jabot_data$recordedBy.new)
  
  # Collector number
  jabot_data$recordNumber.new <- plantR::colNumber(jabot_data$recordNumber, noNumb = "s.n.")
  
  # Collection and identification year #### Check with Renato: plantR::getYear("01/02/99") ####
  # if we use as.Date("01/02/99", format = "%d/%m/%y") we get 1999 but this may well be 1899...
  
  jabot_data$year.new <- plantR::getYear(jabot_data$year, noYear = "n.d.")
  jabot_data$yearIdentified.new <- plantR::getYear(jabot_data$dateIdentified, noYear = "n.d.")
  
  
  # Data editing ------------------------------------------------------------
  
  
  # Formatting locality information at different scales
  jabot_data <- plantR::fixLoc(jabot_data,
                                loc.levels = c("country",
                                               "stateProvince", 
                                               "municipality", 
                                               "locality"),
                                scrap = TRUE)
  
  # Get and simplify locality string create by strLoc
  locs <- plantR::strLoc(jabot_data)
  
  # Simplify
  locs$loc.string <- plantR::prepLoc(locs$loc.string) # priority string
  locs$loc.string1 <- plantR::prepLoc(locs$loc.string1) # alternative string 1
  locs$loc.string2 <- plantR::prepLoc(locs$loc.string2) # alternative string 2
  
  # Cross the simplified strings against plantR's gazetteer
  locs <- plantR::getLoc(locs, gazet = "plantR")
  
  # Bind the information obtained from the gazetteer into jabot_data
  jabot_data <- cbind.data.frame(jabot_data, locs[, c("loc",
                                                        "loc.correct",
                                                        "latitude.gazetteer",
                                                        "longitude.gazetteer",
                                                        "resolution.gazetteer")])
  
  
  ##### Editing geographical coordinates #####
  
  # Transforms the coordinates into decimal degrees
  jabot_data <- plantR::prepCoord(jabot_data)
  
  # Missing coordinates can be obtained from the gazetteer
  jabot_data <- plantR::getCoord(jabot_data)
  
  
  # Skipping formatTax steps since we already used our taxonomy to fix the names
  
  
  # Data validation ---------------------------------------------------------
  
  
  # Validating locality information
  jabot_data <- plantR::validateLoc(jabot_data)
  
  # Validating original coordinates at country, state, and county level
  jabot_data <- plantR::checkCoord(jabot_data,
                                    keep.cols = c("geo.check",
                                                  "NAME_0",
                                                  "country.gazet"))
  
  # Validating coordinates due to rounding precision
  jabot_data <- plantR::checkBorders(jabot_data, output = "same.col")
  
  # Validating coordinates in the sea but close to the shore
  jabot_data <- plantR::checkShore(jabot_data, output = "same.col")
  
  # Validating inverted and swapped coordinates
  jabot_data <- plantR::checkInverted(jabot_data, output = "same.col")
  
  # Validating records from cultivated individuals
  jabot_data <- plantR::getCult(jabot_data)
  
  # Validating spatial outliers
  jabot_data <-  plantR::checkOut(jabot_data, 
                                   tax.name = "scientificName",
                                   clas.cut = 2.5,
                                   rob.cut = 12,
                                   keep.values = TRUE)
  
  # Validating records that are falling outside of the species' indicated botanical country
  jabot_data <- plantR::checkDist(x = jabot_data,
                                   tax.name = "taxonName",
                                   tax.author = "taxonAuthor",
                                   loc = "loc.correct",
                                   source = "wcvp")
  
  # Validating confidence level of species determinators
  jabot_data <- plantR::validateTax(jabot_data)
  
  # Saving by collection and separating by family
  plantR::saveData(jabot_data,
                   dir.name = i,
                   path = file_path,
                   by = "family",
                   file.format = "rds",
                   compress = TRUE)
}
