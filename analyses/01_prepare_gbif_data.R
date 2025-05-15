
# Create the derived-data folders for GBIF data
file_path <- here::here("data", "derived-data", "GBIF_occ")
if(!dir.exists(file_path))
  dir.create(file_path)

# Loading all family names presented in our checklist
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))

# Loading our taxonomy to fix the names
our_names <- readRDS(file.path(here::here("data", "derived-data", "checklist_standardized.rds")))

# Remove families already prepared, if any
# families_done <- list.dirs(here::here("data", "derived-data", "GBIF_occ"), 
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

# Loading internal functions
devtools::load_all()

# Preparing GBIF data -----------------------------------------------------

for(i in family_names){

  cat("Preparing", i, "\n")
  
  # GBIF data
  file_path2 <- "D:/gbif_occurrences.txt"
  # Prepare the header first
  header <- readLines(file_path2, n = 1)
  # Split the header into column names
  header <- unlist(strsplit(header, "\t"))
  
  # Load each family separately (family is column $36)
  gbif_data <- data.table::fread(cmd = paste("awk -F'\t' '$36 == \"", i, "\"'", " ", 
                                             file_path2, sep = ""), quote = "")
  
  # If data loaded is NULL or has no rows
  if (is.null(gbif_data) || nrow(gbif_data) == 0) {
    cat("Skipping", " ", i, ":", " no registers found\n", sep = "")
    dir.create(file.path(file_path, i), showWarnings = FALSE)
    next
  }
  
  # set the data.frame names from the header object
  data.table::setnames(gbif_data, header)

  # Convert the data.table to data.frame for plantR
  gbif_data <- as.data.frame(gbif_data)

  # Keep only rows that contain a scientificName found in our taxonomy input_full_name
  gbif_data <- gbif_data[gbif_data$scientificName %in% our_names$input_full_name, ]
  
  if (is.null(gbif_data) || nrow(gbif_data) == 0) {
    cat("Skipping", " ", i, ":", " no registers found\n", sep = "")
    dir.create(file.path(file_path, i), showWarnings = FALSE)
    next
  }

  # Match and substitute the gbif name by our checklist name, already manually checked
  match_indices <- match(gbif_data$scientificName, our_names$input_full_name)
  gbif_data$scientificName <- our_names$our_accepted_scientificName[match_indices]
  gbif_data$family <- our_names$our_accepted_taxonFamily[match_indices]
  gbif_data$taxonAuthor <- our_names$our_accepted_taxonAuthorship[match_indices]
  gbif_data$taxonName <- our_names$our_accepted_taxonName[match_indices]
  
  # Names that became NA are species that are not in our checklist, i.e., species from outside
  # of the Neotropics; so remove
  gbif_data <- gbif_data[!is.na(gbif_data$scientificName), ]
  
  # Adding the scientificNameStatus column from fixSpecies here
  gbif_data$scientificNameStatus <- plantR::fixSpecies(x = gbif_data,
                                                       tax.name = "taxonName",
                                                       author.name = "scientificNameAuthorship")$scientificNameStatus
  
  # Get country name from countryCode
  gbif_data$country <- countrycode::countrycode(sourcevar = gbif_data$countryCode,
                                                origin = "iso2c",
                                                destination = "country.name")
  # Data formatting in DwC
  gbif_data <- plantR::formatDwc(gbif_data = gbif_data)
  
  
    # Data standardization ----------------------------------------------------
    
    cat("Standardizing data \n")  
  
    ##### Standardizing collection codes #####
    gbif_data <- plantR::getCode(gbif_data) # later update plantR:::collectionCodes with 00_get_collections.R
    
    ##### Standardizing names of collectors and determiners #####
    
    # First collector
    gbif_data$recordedBy.new <- my_prepName(gbif_data$recordedBy,
                                                 output = "first",
                                                 sep.out = "; ")
    # Auxilliary collectors
    gbif_data$recordedBy.aux <- my_prepName(gbif_data$recordedBy,
                                                 output = "aux",
                                                 sep.out = "; ")
    # First determiner
    gbif_data$identifiedBy.new <- my_prepName(gbif_data$identifiedBy,
                                                   output = "first",
                                                   sep.out = "; ")
    # Auxilliary determiners
    gbif_data$identifiedBy.aux <- my_prepName(gbif_data$identifiedBy,
                                                   output = "aux",
                                                   sep.out = "; ")
    
    # Name notations
    gbif_data$recordedBy.new <- plantR::missName(gbif_data$recordedBy.new,
                                                 type = "collector",
                                                 noName = "s.n")
    gbif_data$identifiedBy.new <- plantR::missName(gbif_data$identifiedBy.new,
                                                   type = "determiner",
                                                   noName = "s.n")
    
    # Extract the last name of the collector to future standardization
    gbif_data$last.name <- plantR::lastName(gbif_data$recordedBy.new)
    
    # Collector number
    gbif_data$recordNumber.new <- plantR::colNumber(gbif_data$recordNumber, noNumb = "s.n.")
    
    # Collection and identification year #### Check with Renato: plantR::getYear("01/02/99") ####
    # if we use as.Date("01/02/99", format = "%d/%m/%y") we get 1999 but this may well be 1899...
    
    gbif_data$year.new <- plantR::getYear(gbif_data$year, 
                                          noYear = "n.d.")
    gbif_data$yearIdentified.new <- plantR::getYear(gbif_data$dateIdentified, 
                                                    noYear = "n.d.")
    
    # Data editing ------------------------------------------------------------
    
    cat("Editing data \n")
    
    # Formatting locality information at different scales
    gbif_data <- plantR::fixLoc(gbif_data,
                                loc.levels = c("country",
                                               "stateProvince", 
                                               "municipality", 
                                               "locality"),
                                scrap = TRUE)
    
    # Get and simplify locality string create by strLoc
    locs <- plantR::strLoc(gbif_data)
    
    # Simplify
    locs$loc.string <- plantR::prepLoc(locs$loc.string) # priority string
    locs$loc.string1 <- plantR::prepLoc(locs$loc.string1) # alternative string 1
    locs$loc.string2 <- plantR::prepLoc(locs$loc.string2) # alternative string 2
    
    # Cross the simplified strings against plantR's gazetteer
    locs <- plantR::getLoc(locs, gazet = "plantR")
    
    # Bind the information obtained from the gazetteer into gbif_data
    gbif_data <- cbind.data.frame(gbif_data, locs[, c("loc",
                                                      "loc.correct",
                                                      "latitude.gazetteer",
                                                      "longitude.gazetteer",
                                                      "resolution.gazetteer")])
    

    ##### Editing geographical coordinates #####
    
    # Transforms the coordinates into decimal degrees
    gbif_data <- plantR::prepCoord(gbif_data)
    
    # Missing coordinates can be obtained from the gazetteer
    gbif_data <- plantR::getCoord(gbif_data)
    
    
    # Skipping formatTax steps since we already used our taxonomy to fix the names
    
    
    # Data validation ---------------------------------------------------------
    
    cat("Validating data \n")
    
    # Validating locality information
    gbif_data <- plantR::validateLoc(gbif_data)
    
    # Validating original coordinates at country, state, and county level
    gbif_data <- plantR::checkCoord(gbif_data,
                                    keep.cols = c("geo.check", 
                                                  "NAME_0",
                                                  "country.gazet"))
    
    # Validating coordinates due to rounding precision
    gbif_data <- plantR::checkBorders(gbif_data, output = "same.col")
    
    # Validating coordinates in the sea but close to the shore
    gbif_data <- plantR::checkShore(gbif_data, output = "same.col")
    
    # Validating inverted and swapped coordinates
    gbif_data <- plantR::checkInverted(gbif_data, output = "same.col")
    
    # Validating records from cultivated individuals
    gbif_data <- plantR::getCult(gbif_data)
    
    # Validating spatial outliers
    gbif_data <- plantR::checkOut(gbif_data, tax.name = "scientificName")
    
    # Validating records that are falling outside of the species' indicated botanical country
    gbif_data <- plantR::checkDist(x = gbif_data,
                                   tax.name = "taxonName",
                                   tax.author = "taxonAuthor",
                                   loc = "loc.correct",
                                   source = "wcvp")
    
    # Validating confidence level of species determinators
    gbif_data <- plantR::validateTax(gbif_data)
    
    # Saving by collection and separating by family 
    plantR::saveData(gbif_data,
                     dir.name = i,
                     path = file_path,
                     by = "family",
                     file.format = "rds",
                     compress = TRUE)
    }
      