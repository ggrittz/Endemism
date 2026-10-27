
# Loading internal functions
devtools::load_all()

# Create the derived-data folders for REFLORA data
file_path <- here::here("data", "derived-data", "REFLORA_occ")
if(!dir.exists(file_path))
  dir.create(file_path)

# Loading all family names presented in our checklist
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))
# family_names <- toupper(family_names)

# Loading our taxonomy to fix the names
our_names <- readRDS(file.path(here::here("data", "derived-data", "checklist_species_level.rds")))

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

# Loading data (already prepared)
file_path2 <- here::here("data", "raw-data", "REFLORA", "reflora_occurrence_filtered.txt")
reflora_data <- as.data.frame(data.table::fread(file_path2))


# Preparing REFLORA data ----------------------------------------------------

for(i in family_names){
  
  cat("Preparing", i, "\n")
  
  reflora_data1 <- reflora_data[reflora_data$family %in% i, ]
  
  # If data loaded is NULL or has no rows
  if (is.null(reflora_data1) || nrow(reflora_data1) == 0) {
    cat("Skipping", " ", i, ":", " no registers found\n", sep = "")
    dir.create(file.path(file_path, i), showWarnings = FALSE)
    next
  }
  
  # Data formatting in DwC
  reflora_data1 <- plantR::formatDwc(user_data = reflora_data1)
  
  
  # Data standardization ----------------------------------------------------
  
  
  ##### Standardizing collection codes #####
  reflora_data1 <- plantR::getCode(reflora_data1) # later update plantR:::collectionCodes with 00_get_collections.R
  
  ##### Standardizing names of collectors and determiners #####
  
  # First collector
  reflora_data1$recordedBy.new <- plantR::prepName(reflora_data1$recordedBy,
                                                 output = "first",
                                                 sep.out = "; ")
  # Auxilliary collectors
  reflora_data1$recordedBy.aux <- plantR::prepName(reflora_data1$recordedBy,
                                                 output = "aux",
                                                 sep.out = "; ")
  # First determiner
  reflora_data1$identifiedBy.new <- plantR::prepName(reflora_data1$identifiedBy,
                                                   output = "first",
                                                   sep.out = "; ")
  # Auxilliary determiners
  reflora_data1$identifiedBy.aux <- plantR::prepName(reflora_data1$identifiedBy,
                                                   output = "aux",
                                                   sep.out = "; ")
  
  # Name notations
  reflora_data1$recordedBy.new <- plantR::missName(reflora_data1$recordedBy.new,
                                                 type = "collector",
                                                 noName = "s.n")
  reflora_data1$identifiedBy.new <- plantR::missName(reflora_data1$identifiedBy.new,
                                                   type = "determiner",
                                                   noName = "s.n")
  
  # Extract the last name of the collector to future standardization
  reflora_data1$last.name <- plantR::lastName(reflora_data1$recordedBy.new)
  
  # Collector number
  reflora_data1$recordNumber.new <- plantR::colNumber(reflora_data1$recordNumber, noNumb = "s.n.")
  
  # Collection and identification year #### Check with Renato: plantR::getYear("01/02/99") ####
  # if we use as.Date("01/02/99", format = "%d/%m/%y") we get 1999 but this may well be 1899...
  
  reflora_data1$year.new <- plantR::getYear(reflora_data1$year, noYear = "n.d.")
  reflora_data1$yearIdentified.new <- plantR::getYear(reflora_data1$dateIdentified, noYear = "n.d.")
  
  
  # Data editing ------------------------------------------------------------
  
  
  # Formatting locality information at different scales
  reflora_data1 <- plantR::fixLoc(reflora_data1,
                                loc.levels = c("country",
                                               "stateProvince", 
                                               "municipality", 
                                               "locality"),
                                scrap = TRUE)
  
  # Get and simplify locality string create by strLoc
  locs <- plantR::strLoc(reflora_data1)
  
  # Simplify
  locs$loc.string <- plantR::prepLoc(locs$loc.string) # priority string
  locs$loc.string1 <- plantR::prepLoc(locs$loc.string1) # alternative string 1
  locs$loc.string2 <- plantR::prepLoc(locs$loc.string2) # alternative string 2
  
  # Cross the simplified strings against plantR's gazetteer
  locs <- plantR::getLoc(locs, gazet = "plantR")
  
  # Bind the information obtained from the gazetteer into reflora_data1
  reflora_data1 <- cbind.data.frame(reflora_data1, locs[, c("loc",
                                                        "loc.correct",
                                                        "latitude.gazetteer",
                                                        "longitude.gazetteer",
                                                        "resolution.gazetteer")])
  
  
  ##### Editing geographical coordinates #####
  
  # Transforms the coordinates into decimal degrees
  reflora_data1 <- plantR::prepCoord(reflora_data1)
  
  # Missing coordinates can be obtained from the gazetteer
  reflora_data1 <- plantR::getCoord(reflora_data1)
  
  
  # Skipping formatTax steps since we already used our taxonomy to fix the names
  
  
  # Data validation ---------------------------------------------------------
  
  
  # Validating locality information
  reflora_data1 <- plantR::validateLoc(reflora_data1)
  
  # Validating original coordinates at country, state, and county level
  reflora_data1 <- plantR::checkCoord(reflora_data1,
                                    keep.cols = c("geo.check",
                                                  "NAME_0",
                                                  "country.gazet"))
  
  # Validating coordinates due to rounding precision
  reflora_data1 <- plantR::checkBorders(reflora_data1, output = "same.col")
  
  # Validating coordinates in the sea but close to the shore
  reflora_data1 <- plantR::checkShore(reflora_data1, output = "same.col")
  
  # Validating inverted and swapped coordinates
  reflora_data1 <- plantR::checkInverted(reflora_data1, output = "same.col")
  
  # Validating records from cultivated individuals
  reflora_data1 <- plantR::getCult(reflora_data1)
  
  # Validating spatial outliers
  reflora_data1 <-  plantR::checkOut(reflora_data1, 
                                   tax.name = "scientificName",
                                   clas.cut = 2.5,
                                   rob.cut = 12,
                                   keep.values = TRUE)
  
  # Validating records that are falling outside of the species' indicated botanical country
  # Temporary location column with country/state names as written in WGSRPD
  loc_state <- sub("^([^_]*_[^_]*).*", "\\1", reflora_data1$loc.correct)
  fix_these <- grepl("_", loc_state) & loc_state %in% names(loc_alias)
  reflora_data1$loc.dist <- reflora_data1$loc.correct
  reflora_data1$loc.dist[fix_these] <- paste0(sub("_.*", "_", loc_state[fix_these]),
                                        loc_alias[loc_state[fix_these]],
                                        sub("^[^_]*_[^_]*", "", reflora_data1$loc.correct[fix_these]))
  loc_country <- sub("_.*", "", reflora_data1$loc.dist)
  fix_these <- loc_country %in% names(loc_alias)
  reflora_data1$loc.dist[fix_these] <- paste0(loc_alias[loc_country[fix_these]],
                                        sub("^[^_]*", "", reflora_data1$loc.dist[fix_these]))
  reflora_data1 <- plantR::checkDist(x = reflora_data1,
                                tax.name = "taxonName",
                                tax.author = "taxonAuthor",
                                loc = "loc.dist",
                                source = "wcvp")
  reflora_data1$loc.dist <- NULL
  
  # Validating confidence level of species determinators
  # Error in col["order"] : 
  #  objeto de tipo 'closure' não possível dividir em subconjuntos
  reflora_data1 <- plantR::validateTax(reflora_data1, 
                                       col.names = c(# class = "class", 
                                                     # order = "order", # issue here send Renato
                                                     family = "family.new", 
                                                     det.name = "identifiedBy.new", 
                                                     col.name = "recordedBy.new", 
                                                     types = "typeStatus", 
                                                     rec.ID = "numTombo", 
                                                     rec.type = "basisOfRecord"),
                                       generalist = FALSE, print = FALSE)
  
  # Considering low complexity ID as "high"
  ###### Gotta add this in the next version #####
  
  # Saving by collection and separating by family
  plantR::saveData(reflora_data1,
                   dir.name = i,
                   path = file_path,
                   by = "family",
                   file.format = "rds",
                   compress = TRUE)
}
