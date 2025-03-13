
# Create the derived-data folders for speciesLink data
file_path <- here::here("data", "derived-data", "speciesLink_occ")
if(!dir.exists(file_path))
  dir.create(file_path)

# Loading all family names presented in our checklist
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))

# Loading our taxonomy to fix the names
our_names <- readRDS(file.path(here::here("data", "derived-data", "checklist_standardized.rds")))

# Remove families already prepared, if any
families_done <- list.dirs(here::here("data", "derived-data", "speciesLink_occ"), 
                           full.names = FALSE)
families_done <- families_done[!grepl("^$", families_done)]
family_names <- setdiff(family_names, families_done)

# Pre-loading objects that will be needed in the pipeline below
islandsBuff <- plantR::islandsBuff
landBuff <- plantR::landBuff
latamMap <- plantR::latamMap
shoreLines <- plantR::shoreLines
world <- plantR::world
worldMap <- plantR::worldMap

# Make world and worldMap have the same units, i.e., add the missing caspian sea
setdiff(worldMap$NAME_0, world$name)
get_these <- which(worldMap$NAME_0 %in% "caspian sea")
caspian <- worldMap[get_these, ]
names(caspian) <- names(world)
world <- rbind(world, caspian)

# Loading internal functions
devtools::load_all()


# Preparing speciesLink data ----------------------------------------------

for(i in family_names){
  
  cat("Preparing", i, "\n")
  
  # speciesLink data
  file_path2 <- "D:/speciesLink_occurrences.txt"
  # Prepare the header first
  header <- readLines(file_path2, n = 1)
  # Split the header into column names
  header <- unlist(strsplit(header, "\t"))
  
  # Load each family separately (family is column $8)
  splink_data <- data.table::fread(cmd = paste("awk -F'\t' '$8 == \"", i, "\"'", " ", 
                                             file_path2, sep = ""), quote = "")
  
  # If data loaded is NULL or has no rows
  if (is.null(splink_data) || nrow(splink_data) == 0) {
    cat("Skipping", " ", i, ":", " no registers found\n", sep = "")
    dir.create(file.path(file_path, i), showWarnings = FALSE)
    next
  }
  
  # set the data.frame names from the header object
  data.table::setnames(splink_data, header)
  
  # Convert the data.table to data.frame for plantR
  splink_data <- as.data.frame(splink_data)
  
  # Create full name column
  splink_data$scientificname_full <- ifelse(is.na(splink_data$scientificnameauthor),
                                            splink_data$scientificname,
                                            paste(splink_data$scientificname,
                                                  splink_data$scientificnameauthor))
  
  # Match with the cleaned version
  match_indices <- match(.clean_char(splink_data$scientificname_full),
                         .clean_char(our_names$input_full_name))
  
  # Keep only rows that contain a cleaned scientific name found in our taxonomy
  splink_data <- splink_data[!is.na(match_indices), ]
  
  if (is.null(splink_data) || nrow(splink_data) == 0) {
    cat("Skipping", " ", i, ":", " no registers found\n", sep = "")
    dir.create(file.path(file_path, i), showWarnings = FALSE)
    next
  }
  
  # For these ones, match again and get our checklist names
  match_indices <- match(.clean_char(splink_data$scientificname_full),
                         .clean_char(our_names$input_full_name))
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
  
  # Adding the scientificNameStatus column from fixSpecies here
  splink_data$scientificNameStatus <- plantR::fixSpecies(x = splink_data,
                                                       tax.name = "taxonName",
                                                       author.name = "taxonAuthor")$scientificNameStatus
  
  # Data formatting in DwC
  splink_data <- plantR::formatDwc(splink_data = splink_data)
  
  
  # # For very large data sets, process by genus and bind it all back before saving by family
  # if(dim(splink_data)[1] >= 2000000){
  #   
  #   gc()
  #   
  #   cat("Data set is too big. Processing family", i, "by genus.", "\n")
  #   
  #   # Vector of genus to loop
  #   genus_names <- sort(unique(splink_data$genus))
  #   #genus_done <- readRDS("data/derived-data/asteraceae_genus.rds")
  #   #genus_done2 <- readRDS("data/derived-data/asteraceae_genus2.rds")
  #   #genus_done <- unique(c(names(genus_done), unique(genus_done2$genus)))
  #   # genus_names <- setdiff(genus_names, genus_done)
  #   #rm(genus_done)
  #   #rm(genus_done2)
  #   #gc()
  #   
  #   # Empty list to merge it all back together before saving
  #   genus_df <- list()
  #   
  #   # Loop by genus
  #   for(j in genus_names){
  #     # genus_names[17:length(genus_names)]
  #     
  #     cat("Processing", j, "\n")
  #     
  #     genus_data <- splink_data[splink_data$genus %in% j, ]
  #     # genus_data <- genus_data[genus_data$gbifID %in% c(3986054033, 1697844331), ]
  #     
  #       if (is.null(genus_data) || nrow(genus_data) == 0) {
  #       cat("Genus", i, "not found. Skipping it.")
  #       next
  #     }
  #     
  #     
  #     # Data standardization ----------------------------------------------------
  #     
  #     
  #     ##### Standardizing collection codes #####
  #     genus_data <- plantR::getCode(genus_data) # later update plantR:::collectionCodes with 00_get_collections.R
  #     
  #     ##### Standardizing names of collectors and determiners #####
  #     
  #     # First collector
  #     genus_data$recordedBy.new <- my_prepName(genus_data$recordedBy,
  #                                              output = "first",
  #                                              sep.out = "; ")
  #     # Auxilliary collectors
  #     genus_data$recordedBy.aux <- my_prepName(genus_data$recordedBy,
  #                                              output = "aux",
  #                                              sep.out = "; ")
  #     # First determiner
  #     genus_data$identifiedBy.new <- my_prepName(genus_data$identifiedBy,
  #                                                output = "first",
  #                                                sep.out = "; ")
  #     # Auxilliary determiners
  #     genus_data$identifiedBy.aux <- my_prepName(genus_data$identifiedBy,
  #                                                output = "aux",
  #                                                sep.out = "; ")
  #     
  #     # Name notations
  #     genus_data$recordedBy.new <- plantR::missName(genus_data$recordedBy.new,
  #                                                   type = "collector",
  #                                                   noName = "s.n")
  #     genus_data$identifiedBy.new <- plantR::missName(genus_data$identifiedBy.new,
  #                                                     type = "determiner",
  #                                                     noName = "s.n")
  #     
  #     # Extract the last name of the collector to future standardization
  #     genus_data$last.name <- plantR::lastName(genus_data$recordedBy.new)
  #     
  #     # Collector number
  #     genus_data$recordNumber.new <- plantR::colNumber(genus_data$recordNumber, noNumb = "s.n.")
  #     
  #     # Collection and identification year #### Check with Renato: plantR::getYear("01/02/99") ####
  #     # if we use as.Date("01/02/99", format = "%d/%m/%y") we get 1999 but this may well be 1899...
  #     
  #     genus_data$year.new <- plantR::getYear(genus_data$year, noYear = "n.d.")
  #     genus_data$yearIdentified.new <- plantR::getYear(genus_data$dateIdentified, noYear = "n.d.")
  #     
  #     
  #     # Data editing ------------------------------------------------------------
  #     
  #     
  #     # Formatting locality information at different scales
  #     if (nrow(genus_data) == 1) {
  #       genus_data <- plantR::fixLoc(genus_data,
  #                                    loc.levels = c("country",
  #                                                   "stateProvince",
  #                                                   "municipality",
  #                                                   "locality"),
  #                                    scrap = FALSE)
  #       genus_data$locality.scrap <- NA
  #     } else {
  #       genus_data <- plantR::fixLoc(genus_data,
  #                                    loc.levels = c("country",
  #                                                   "stateProvince",
  #                                                   "municipality",
  #                                                   "locality"),
  #                                    scrap = TRUE)
  #     }
  #     
  #     # Get and simplify locality string create by strLoc
  #     locs <- plantR::strLoc(genus_data)
  #     
  #     # Simplify
  #     locs$loc.string <- plantR::prepLoc(locs$loc.string) # priority string
  #     locs$loc.string1 <- plantR::prepLoc(locs$loc.string1) # alternative string 1
  #     if("locality.scrap" %in% names(genus_data))
  #     locs$loc.string2 <- plantR::prepLoc(locs$loc.string2) # alternative string 2
  #     
  #     # Cross the simplified strings against plantR's gazetteer
  #     locs <- plantR::getLoc(locs, gazet = "plantR")
  #     
  #     # Bind the information obtained from the gazetteer into genus_data
  #     genus_data <- cbind.data.frame(genus_data, locs[, c("loc",
  #                                                         "loc.correct",
  #                                                         "latitude.gazetteer",
  #                                                         "longitude.gazetteer",
  #                                                         "resolution.gazetteer")])
  #     
  # 
  #     ##### Editing geographical coordinates #####
  #     
  #     # Transforms the coordinates into decimal degrees
  #     genus_data <- plantR::prepCoord(genus_data)
  #     
  #     # Missing coordinates can be obtained from the gazetteer
  #     genus_data <- plantR::getCoord(genus_data)
  #     
  #     
  #     # Skipping formatTax steps since we already used our taxonomy to fix the names
  #     
  #     
  #     # Data validation ---------------------------------------------------------
  #     
  #     
  #     # Validating locality information
  #     genus_data <- plantR::validateLoc(genus_data)
  #     
  #     # Validating original coordinates at country, state, and county level
  #     genus_data <- plantR::checkCoord(genus_data,
  #                                      keep.cols = c("geo.check",
  #                                                    "NAME_0",
  #                                                    "country.gazet"))
  #     
  #     # Validating coordinates due to rounding precision
  #     genus_data <- my_checkBorders(genus_data, output = "same.col")
  #     
  #     # Validating coordinates in the sea but close to the shore
  #     genus_data <- plantR::checkShore(genus_data, output = "same.col")
  #     
  #     # Validating inverted and swapped coordinates
  #     genus_data <- my_checkInverted(genus_data, output = "same.col")
  #     
  #     # Validating records from cultivated individuals
  #     genus_data <- plantR::getCult(genus_data)
  #     
  #     # Really specific cases where there are no valid coordinate nor
  #     # a country/stateProvince/locality to extract coordinates from the gazetteer
  #     if (all(is.na(genus_data$decimalLongitude.new) | 
  #             is.na(genus_data$decimalLatitude.new))) {
  #       genus_data$out.check <- "no.coords"
  #     } else {
  #       genus_data <- plantR::checkOut(genus_data, tax.name = "scientificName")
  #     }
  #     
  #     # Validating records that are falling outside of the species' indicated botanical country
  #     genus_data <- checkDist(genus_data,
  #                             tax.names = c("taxonName", "taxonAuthor"),
  #                             loc = "loc.correct",
  #                             source = "wcvp")
  #     
  #     # Validating confidence level of species determinators
  #     genus_data <- plantR::validateTax(genus_data)
  #     
  #     # Bind it all back before merging and saving
  #     genus_df[[j]] <- genus_data
  #     
  # }
  # 
  #   # Bind it all back
  #   genus_df <- do.call(plyr::rbind.fill, genus_df)
  #   
  #   # Saving by collection and separating by family
  #   plantR::saveData(genus_df,
  #                    dir.name = i,
  #                    path = file_path,
  #                    by = "family",
  #                    file.format = "rds",
  #                    compress = TRUE)
  #   
  #   
  #   } else {
  
  
  # Data standardization ----------------------------------------------------
  
  
  ##### Standardizing collection codes #####
  splink_data <- plantR::getCode(splink_data) # later update plantR:::collectionCodes with 00_get_collections.R
  
  ##### Standardizing names of collectors and determiners #####
  
  # First collector
  splink_data$recordedBy.new <- my_prepName(splink_data$recordedBy,
                                          output = "first",
                                          sep.out = "; ")
  # Auxilliary collectors
  splink_data$recordedBy.aux <- my_prepName(splink_data$recordedBy,
                                          output = "aux",
                                          sep.out = "; ")
  # First determiner
  splink_data$identifiedBy.new <- my_prepName(splink_data$identifiedBy,
                                            output = "first",
                                            sep.out = "; ")
  # Auxilliary determiners
  splink_data$identifiedBy.aux <- my_prepName(splink_data$identifiedBy,
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
  splink_data <- my_checkCoord(splink_data,
                               keep.cols = c("geo.check",
                                             "NAME_0",
                                             "country.gazet"))
  
  # Validating coordinates due to rounding precision
  splink_data <- my_checkBorders(splink_data, output = "same.col")
  
  # Validating coordinates in the sea but close to the shore
  splink_data <- plantR::checkShore(splink_data, output = "same.col")
  
  # Validating inverted and swapped coordinates
  splink_data <- my_checkInverted(splink_data, output = "same.col")
  
  # Validating records from cultivated individuals
  splink_data <- plantR::getCult(splink_data)
  
  # Validating spatial outliers
  # Really specific cases where there are no valid coordinate nor
  # a country/stateProvince/locality to extract coordinates from the gazetteer
  if (all(is.na(splink_data$decimalLongitude.new) | 
          is.na(splink_data$decimalLatitude.new))) {
    splink_data$out.check <- "no.coords"
  } else {
    splink_data <- plantR::checkOut(splink_data, tax.name = "scientificName")
  }
  
  # Validating records that are falling outside of the species' indicated botanical country
  splink_data <- checkDist(splink_data, 
                         tax.names = c("taxonName", "taxonAuthor"),
                         loc = "loc.correct",
                         source = "wcvp")
  
  # Validating confidence level of species determinators
  splink_data <- plantR::validateTax(splink_data)
  
  # Saving by collection and separating by family
  plantR::saveData(splink_data,
                   dir.name = i,
                   path = file_path,
                   by = "family",
                   file.format = "rds",
                   compress = TRUE)
}
