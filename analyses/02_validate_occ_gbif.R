
# Validating occurrences from GBIF ----------------------------------------

# Pre-loading latamMap and worldMap objects
latamMap <- plantR::latamMap
worldMap <- plantR::worldMap
world <- plantR::world
landBuff <- plantR::landBuff
islandBuff <- plantR::islandsBuff
shoreLines <- plantR::shoreLines

# Loading path with info
file_path <- here::here("data", "derived-data", "GBIF_collections", "edited")

# Get all collection names
collections <- list.files(file_path)

# Vector to store families
all_families <- c()

# Loop through each collection and get all family names each one of them have
for (j in collections) {
  
  collection_path <- file.path(file_path, j)
  rds_files <- list.files(collection_path, pattern = "\\.rds$")
  all_families <- c(all_families, rds_files)
}

# Get unique family names
all_families <- sort(unique(all_families))



# Data validation procedure -----------------------------------------------

#### Preparing data to be looped by family

for (i in all_families) {
  # Store data from each collection here
  collection_data_list <- list()
  
  # Loop through each collection
  for (j in collections) {
    
    collection_path <- file.path(file_path, j)
    # Check if the target family exists in the collection
    if (!(i %in% list.files(collection_path))) {
      # If it doesn't, skip
      next
    }
    
    rds_path <- file.path(collection_path, i)
    family_data <- readRDS(rds_path)
    
    # Append the data to the collections list
    collection_data_list[[j]] <- family_data
  }
  
    # Combine all the data for the family
    family_prepared <- do.call(rbind, collection_data_list)
    rownames(family_prepared) <- NULL
    
    
    #### Validating geography ####
    
    # Step 1. Locality information
    family_prepared <- plantR::validateLoc(family_prepared)
    
    # Categories that may need to be removed: considering only the country info
    # to_remove <- c("check_country2no.info", "check_local.2country",
    #                "check_municip.2country", "check_state2country")
    
    # Step 2. Geographic coordinates
    # Validate coordinates at country, state, and county level
    # Package needs to be loaded here due to plantR::latamMap and plantR::worldMap
    # Or we can simply create the objects with the same name as needed
    family_prepared <- plantR::checkCoord(family_prepared,
                                          keep.cols = c("geo.check",
                                                        "NAME_0",
                                                        "country.gazet"))
    
    
    # Step 3. Coordinates falling in neighbouring country
    family_prepared <- plantR::checkBorders(family_prepared,
                                            output = "same.col")
    
    
    # Step 4. Coordinates falling into the sea but close to the shore
    family_prepared <- plantR::checkShore(family_prepared,
                                          output = "same.col")
    
    # Step 5. Inverted and swapped coordinates
    family_prepared <- plantR::checkInverted(family_prepared,
                                             output = "same.col")
    
    # Step 6. Cultivated individuals
    family_prepared <- plantR::getCult(family_prepared)
    
    
    # Step 7. Flagging spatial outliers
    family_prepared <- plantR::checkOut(family_prepared,
                                        tax.name = "scientificName.new")
    
    
    
    # Step 8. Flagging distributions outside of indicated in the literatura
    family_prepared1 <- checkDist(family_prepared)
    family_prepared12 <- checkDist(family_prepared, source = 'wcvp')
    
    
    
    #### Validating taxonomy ####
    
    # Step 1. Confidence level of species determinations
    
    # Add generalists too?
    family_prepared <- plantR::validateTax(family_prepared)
    # table(family_prepared$tax.check)/dim(family_prepared)[1]
    
    
    #### Validating duplicated specimens ####
    
    # Step 1. Create unique ID for each record 
    family_prepared$numTombo <- plantR::getTombo(family_prepared[, c("collectionCode.new")],
                                                 family_prepared[, c("catalogNumber")])
    
    # Step 2. Preparing multiple sets of strings that will be used for searching
    
    # We may need to add more combinations here
    dups <- plantR::prepDup(family_prepared, 
                            comb.fields = list(c("family", "col.last.name", "col.number",
                                            "col.loc"),
                                            c("family", "col.last.name", "col.number", 
                                              "col.year")))
    
    # Step 3. Find duplicates across collections
    dups <- plantR::getDup(dups)
    # table(dups$dup.prop)
    
    # Step 4. Homogenizing the information within duplicates
    # Bind the duplicated information into our data set
    family_prepared <- cbind.data.frame(family_prepared,
                                        dups[, c("dup.ID", "dup.numb", "dup.prop")],
                                        stringsAsFactors = FALSE)
    
    # Merge duplicates
    # Need to add $family.new here in the step related to formatTax!
    family_prepared <- plantR::mergeDup(family_prepared)
    
    # Step 5. Removing duplicates
    family_prepared <- plantR::rmDup(family_prepared)
    
    # Save data by family
    file_path2 <- here::here("data", "derived-data",
                             "GBIF_collections")

    plantR::saveData(df = family_prepared, 
                     path = file_path2,
                     dir.name = "validated",
                     by = "family",
                     file.format = "rds",
                     compress = FALSE)
    
    }



#













