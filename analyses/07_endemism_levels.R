
# Measuring endemism levels -----------------------------------------------

##### Read data #####

# Loading our classified latamMap
file_path <- here::here("data", "derived-data", "maps")
file_path1 <- here::here("data", "derived-data", "Cleaned_occs")
file_path2 <- here::here("data", "derived-data", "Endemic_species")
if(!dir.exists(file_path2))
  dir.create(file_path2)
file_name <- "latamMap_classified.gpkg"
latam_gaz <- terra::vect(file.path(file_path, file_name))

# Load equivalence table for administrative levels that exist in gazetteer
# This is going to be used for keeping records at X, Y, Z adm level
# options(gargle_oauth_cache = ".secrets")
# googlesheets4::gs4_auth() # is this needed?
# googlesheets4::gs4_auth(cache = ".secrets",
#                         email = "ggrittz@usp.br",
#                         scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")
# Download from Google Drive
# link <- "https://docs.google.com/spreadsheets/d/"
# link1 <- "1V42RsVpUQuiHbv-U6U_t--8mCAr7UOjQLpQ9Awq6PRI/edit?gid=0#gid=0"
# eq_table <- as.data.frame(googlesheets4::read_sheet(paste0(link, link1), range = "A:G"))
# eq_table <- eq_table[, c("country", "latamMap_GADM")]

# Fast preparing latam_gaz complete loc.correct1 (NAME_all) in the same way
# it is from loc.correct
latam_gaz <- terra::as.data.frame(latam_gaz, geom = "WKT")
latam_gaz$loc_gaz <- apply(latam_gaz[, 1:5], 1, function(x) {
  non_na <- x[!is.na(x)]
  paste(non_na, collapse = "_")
})

##### Dealing with states too big...validate only if small #####
latamMap <- plantR::latamMap
latamMap <- dplyr::bind_rows(latamMap)

# Convert to terra
latamMap <- terra::vect(latamMap)
# terra::plot(latamMap[latamMap$NAME_1 %in% "santa catarina", ])
# Aggregate everything into country and state-level
latamMap_agg_country <- terra::aggregate(x = latamMap, by = "NAME_0", 
                                         cores = 4, count = FALSE)
latamMap_agg_state <- terra::aggregate(x = latamMap, by = c("NAME_0", "NAME_1"), 
                                 cores = 4, count = FALSE)
# Dealing with holes after agg
latamMap_agg_country <- terra::fillHoles(latamMap_agg_country)
latamMap_agg_state <- terra::fillHoles(latamMap_agg_state)
# Measure the area of each polygon
latamMap_agg_country$area_km <- terra::expanse(latamMap_agg_country, unit = "km")
latamMap_agg_state$area_km <- terra::expanse(latamMap_agg_state, unit = "km")
# Matching column
latamMap_agg_country$name <- paste0(latamMap_agg_country$NAME_0)
latamMap_agg_state$name <- paste(latamMap_agg_state$NAME_0, latamMap_agg_state$NAME_1, sep = "_")
# Do not use a country/state-validated occurrence if its area is above: 12,500 km²
# around 13 % of all latin American states are below 12,500 km², for example
# This value was decided based on the 50 x 50 km grid cells
# At country
latamMap_agg_country$bad_reso <- latamMap_agg_country$area_km >= 12500
# At state
latamMap_agg_state$bad_reso <- latamMap_agg_state$area_km >= 12500
# Df
latamMap_agg_country <- terra::as.data.frame(latamMap_agg_country)
latamMap_agg_state <- terra::as.data.frame(latamMap_agg_state)
# Join
latamMap_agg_adm <- rbind.data.frame(latamMap_agg_country[, c("name", "bad_reso")],
                                     latamMap_agg_state[, c("name", "bad_reso")])
saveRDS(latamMap_agg_adm, here::here("data", "derived-data", "rds", "latamMap_agg_adm.rds"))

# Back to vect
# latam_gaz <- terra::vect(latam_gaz, geom = "geometry", crs = "epsg:4326")

# Morrone 2022 (map dissolved by Realms)
tropical_america <- here::here("data", "derived-data", "maps", "NTAmericas_dissolved.shp")
tropical_america <- terra::vect(tropical_america)
tropical_america <- terra::project(tropical_america, "epsg:4326")
tropical_america <- tropical_america[tropical_america$Morrone_re %in% "Neotropical", ]

# Morrone 2022 (map dissolved by EFGs)
efgs <- here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp")
efgs <- terra::vect(efgs)
efgs <- terra::project(efgs, "epsg:4326")
efgs <- efgs[efgs$Morrone_re %in% "Neotropical", ]

# Gazetteer points
gaz_efg <- terra::vect(here::here("data", "derived-data", "maps", "gaz_intersected_efg.shp"))
gaz_efg <- terra::as.data.frame(gaz_efg, geom = NULL)
gaz_nt <- terra::vect(here::here("data", "derived-data", "maps", "gaz_intersected_nt.shp"))
gaz_nt <- terra::as.data.frame(gaz_nt, geom = NULL)
names(gaz_nt)[which(names(gaz_nt) == "Morrone_re")] <- "is_nt"
gaz_nt$is_nt <- ifelse(gaz_nt$is_nt %in% "Neotropical", TRUE, FALSE)

# Loading family names
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))
# family_names <- family_names[5:311]

# Loading species list
our_names <- readRDS(here::here("data", "derived-data", "checklist_standardized.rds"))
our_names <- our_names[!duplicated(our_names$our_accepted_scientificName), ]


# Get a Realm/EFG for each occurrence -------------------------------------
# And filter only good registers for weighted endemism --------------------


##### ADDING TAXONOMY FIX HERE #####

# Get WFO parentalID for these cases...
raw_path <- here::here("data", "raw-data", "WFO")
# file_name <- "WFO_backbone.zip"
# if (!dir.exists(raw_path)) 
#   dir.create(raw_path)
# options(timeout = 10000)
# download.file(url = "https://zenodo.org/records/18007552/files/_DwC_backbone_R.zip?download=1",
#               destfile = file.path(raw_path, file_name), mode = "wb")
# unzip(file.path(raw_path, file_name), exdir = raw_path)
wfo <- data.table::fread(file.path(raw_path, "classification.csv"))
# wfo$scientificName_full <- ifelse(wfo$scientificNameAuthorship %in% "",
#                                   wfo$scientificName,
#                                   paste(wfo$scientificName, wfo$scientificNameAuthorship))
# Match scientificNames e puxar a informação de parentNameUsageID na coluna taxonID
wfo_dt <- data.table::as.data.table(wfo)

# WCVP now
raw_path1 <- here::here("data", "raw-data", "WCVP")
wcvp <- data.table::fread(file.path(raw_path1, "wcvp_taxon.csv"))
wcvp_dt <- data.table::as.data.table(wcvp)
rm(wfo, wcvp)

# Do it by family
for(i in family_names){
  
  cat("Calculating endemism levels for", i, "\n")
  gz_path <- file.path(file_path1, i, paste0(i, ".csv.gz"))
  
  if(!file.exists(gz_path)){
    cat("Input file not found, skipping \n")
    next
  }
  
  family_data <- data.table::fread(gz_path)
  
  

  # START OF TAXONOMY FIX ---------------------------------------------------
  
  
  
  ##### I AM ADDING THIS FIX HERE, FOR THE MOMENT #####
  ##### THIS FIX IS GOING TO BE ADDED TO TAXONOMY #####
  
  # Infraspecific ranks fix -------------------------------------------------
  
  # Get all occurrences that are infraspecific
  get_these <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                           family_data$taxonName, 
                           perl = TRUE)) 
  
  # Get infraspecific names
  infraspecific_names <- family_data$taxonName[get_these]
  
  # Create a lookup table - ONLY RUN IF THERE ARE INFRASPECIFIC NAMES
  if(length(infraspecific_names) > 0) {
    wfo_lookup <- wfo_dt[scientificName %in% infraspecific_names, 
                         .(scientificName, 
                           scientificNameAuthorship,
                           parentNameUsageID,
                           acceptedNameUsageID,
                           taxonomicStatus,
                           taxonRank)]
    
    # Remove unchecked names
    # wfo_lookup <- wfo_lookup[!wfo_lookup$taxonomicStatus %in% "Unchecked", ]
    
    # Use the acceptedID when parentNameUsageID is empty 
    wfo_lookup[parentNameUsageID == "", parentNameUsageID := acceptedNameUsageID]
    
    # Get parent names for infraspecific taxa
    wfo_parents <- wfo_dt[taxonID %in% wfo_lookup[!is.na(parentNameUsageID) & 
                                                    parentNameUsageID != ""]$parentNameUsageID,
                          .(taxonID, 
                            parent_scientificName = scientificName,
                            parent_scientificNameAuthorship = scientificNameAuthorship)]
    
    wfo_lookup <- dplyr::left_join(wfo_lookup, wfo_parents,
                                   by = c("parentNameUsageID" = "taxonID"))
    
    # Deal with synonyms that caused new accepted names to have subsp./var./f.
    get_these1 <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                              wfo_lookup$parent_scientificName,
                              perl = TRUE))
    
    # Only process problematic cases if they exist
    if(length(get_these1) > 0) {
      # wfo_lookup$parent_scientificName[get_these1]
      
      # Deal with synonyms that caused new accepted names to have subsp./var./f.
      get_these1 <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                                wfo_lookup$parent_scientificName,
                                perl = TRUE))
      
      # Get the parent IDs for these problematic cases
      problem_parent_ids <- wfo_lookup$parentNameUsageID[get_these1]
      
      # Get the parents of these problematic parents (the next level up)
      higher_parents <- wfo_dt[taxonID %in% wfo_dt[taxonID %in% problem_parent_ids]$parentNameUsageID,
                               .(taxonID,
                                 parent_scientificName_fixed = scientificName,
                                 parent_scientificNameAuthorship_fixed = scientificNameAuthorship)]
      
      # Get the mapping of which original parent ID maps to which higher parent
      parent_mapping <- wfo_dt[taxonID %in% problem_parent_ids,
                               .(original_parent_id = taxonID,
                                 higher_parent_id = parentNameUsageID)]
      
      # Join to get the fixed names
      parent_mapping <- dplyr::left_join(parent_mapping, higher_parents,
                                         by = c("higher_parent_id" = "taxonID"))
      
      # Update wfo_lookup
      for(k in seq_along(get_these1)) {
        row_idx <- get_these1[k]
        original_parent_id <- wfo_lookup$parentNameUsageID[row_idx]
        
        match_idx <- which(parent_mapping$original_parent_id == original_parent_id)
        
        if(length(match_idx) > 0) {
          wfo_lookup$parent_scientificName[row_idx] <- parent_mapping$parent_scientificName_fixed[match_idx]
          wfo_lookup$parent_scientificNameAuthorship[row_idx] <- parent_mapping$parent_scientificNameAuthorship_fixed[match_idx]
        }
      }
    }
    
    # When there are duplicates, keep only the accepted
    wfo_lookup <- wfo_lookup[order(taxonomicStatus != "Accepted")]
    wfo_lookup <- wfo_lookup[!duplicated(scientificName), ]
    
    ##### FINALLY SUBSTITUTE THE NAMES #####
    
    # Named vector for quick lookup
    parent_name_lookup <- setNames(wfo_lookup$parent_scientificName, wfo_lookup$scientificName)
    parent_author_lookup <- setNames(wfo_lookup$parent_scientificNameAuthorship, wfo_lookup$scientificName)
    
    # Find matches
    matches <- family_data$taxonName %in% names(parent_name_lookup)
    
    # For rows with matches, update taxonName and taxonAuthor
    if(sum(matches) > 0) {
      # Get the matching scientific names
      matching_names <- family_data$taxonName[matches]
      # Update taxonName with parent_scientificName
      family_data$taxonName[matches] <- parent_name_lookup[matching_names]
      # Update taxonAuthor with parent_scientificNameAuthorship
      family_data$taxonAuthor[matches] <- parent_author_lookup[matching_names]
      # Recreate scientificName column
      family_data$scientificName[matches] <- ifelse(
        !is.na(family_data$taxonAuthor[matches]) & family_data$taxonAuthor[matches] != "",
        paste(family_data$taxonName[matches], family_data$taxonAuthor[matches]),
        family_data$taxonName[matches]
      )
    }
    
    
    
    # Remove unchecked names now (they become NA on taxonName, taxonAuthor etc.)
    # family_data <- family_data[!is.na(family_data$taxonName), ]
  } else {
    cat("  No infraspecific names found for WFO fix in", i, "- skipping\n")
  }
  
  ###########################################################
  ###########################################################
  ##### DOING THE SAME BUT FOR NAMES ONLY FOUND IN WCVP #####
  ###########################################################
  ###########################################################
  
  # Infraspecific ranks fix -------------------------------------------------
  
  # Get all occurrences that are infraspecific
  get_these <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                           family_data$taxonName, 
                           perl = TRUE)) 
  
  # Get infraspecific names
  infraspecific_names <- family_data$taxonName[get_these]
  
  # Create a lookup table - ONLY RUN IF THERE ARE INFRASPECIFIC NAMES
  if(length(infraspecific_names) > 0) {
    wcvp_lookup <- wcvp_dt[scientfiicname %in% infraspecific_names, 
                           .(scientfiicname, 
                             scientfiicnameauthorship,
                             parentnameusageid,
                             acceptednameusageid,
                             taxonomicstatus,
                             taxonrank)]
    
    # Use the acceptedID when parentNameUsageID is empty 
    wcvp_lookup[parentnameusageid == "", parentnameusageid := acceptednameusageid]
    
    # Get parent names for infraspecific taxa
    wcvp_parents <- wcvp_dt[taxonid %in% wcvp_lookup[!is.na(parentnameusageid) & 
                                                       parentnameusageid != ""]$parentnameusageid,
                            .(taxonid, 
                              parent_scientificName = scientfiicname,
                              parent_scientificNameAuthorship = scientfiicnameauthorship)]
    
    wcvp_lookup <- dplyr::left_join(wcvp_lookup, wcvp_parents,
                                    by = c("parentnameusageid" = "taxonid"))
    
    # Deal with synonyms that caused new accepted names to have subsp./var./f.
    get_these1 <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                              wcvp_lookup$parent_scientificName,
                              perl = TRUE))
    
    # Only process problematic cases if they exist
    if(length(get_these1) > 0) {
      # wfo_lookup$parent_scientificName[get_these1]
      
      # Deal with synonyms that caused new accepted names to have subsp./var./f.
      get_these1 <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                                wcvp_lookup$parent_scientificName,
                                perl = TRUE))
      
      # Get the parent IDs for these problematic cases
      problem_parent_ids <- wcvp_lookup$parentnameusageid[get_these1]
      
      # Get the parents of these problematic parents (the next level up)
      higher_parents <- wcvp_dt[taxonid %in% wcvp_dt[taxonid %in% problem_parent_ids]$parentnameusageid,
                                .(taxonid,
                                  parent_scientificName_fixed = scientfiicname,
                                  parent_scientificNameAuthorship_fixed = scientfiicnameauthorship)]
      
      # Get the mapping of which original parent ID maps to which higher parent
      parent_mapping <- wcvp_dt[taxonid %in% problem_parent_ids,
                                .(original_parent_id = taxonid,
                                  higher_parent_id = parentnameusageid)]
      
      # Join to get the fixed names
      parent_mapping <- dplyr::left_join(parent_mapping, higher_parents,
                                         by = c("higher_parent_id" = "taxonid"))
      
      # Update wcvp_lookup
      for(k in seq_along(get_these1)) {
        row_idx <- get_these1[k]
        original_parent_id <- wcvp_lookup$parentnameusageid[row_idx]
        
        match_idx <- which(parent_mapping$original_parent_id == original_parent_id)
        
        if(length(match_idx) > 0) {
          wcvp_lookup$parent_scientificName[row_idx] <- parent_mapping$parent_scientificName_fixed[match_idx]
          wcvp_lookup$parent_scientificNameAuthorship[row_idx] <- parent_mapping$parent_scientificNameAuthorship_fixed[match_idx]
        }
      }
    }
    
    # When there are duplicates, keep only the accepted
    wcvp_lookup <- wcvp_lookup[order(taxonomicstatus != "Accepted")]
    wcvp_lookup <- wcvp_lookup[!duplicated(scientfiicname), ]
    
    ##### FINALLY SUBSTITUTE THE NAMES #####
    
    # Named vector for quick lookup
    parent_name_lookup <- setNames(wcvp_lookup$parent_scientificName, wcvp_lookup$scientfiicname)
    parent_author_lookup <- setNames(wcvp_lookup$parent_scientificNameAuthorship, wcvp_lookup$scientfiicname)
    
    # Find matches
    matches <- family_data$taxonName %in% names(parent_name_lookup)
    
    # For rows with matches, update taxonName and taxonAuthor
    if(sum(matches) > 0) {
      # Get the matching scientific names
      matching_names <- family_data$taxonName[matches]
      # Update taxonName with parent_scientificName
      family_data$taxonName[matches] <- parent_name_lookup[matching_names]
      # Update taxonAuthor with parent_scientificNameAuthorship
      family_data$taxonAuthor[matches] <- parent_author_lookup[matching_names]
      
      # Recreate scientificName column
      family_data$scientificName[matches] <- ifelse(
        !is.na(family_data$taxonAuthor[matches]) & family_data$taxonAuthor[matches] != "",
        paste(family_data$taxonName[matches], family_data$taxonAuthor[matches]),
        family_data$taxonName[matches]
      )
    }
    
    
  } else {
    cat("  No infraspecific names found for WCVP fix in", i, "- skipping\n")
  }
  
  
# END OF TAXONOMY FIX -----------------------------------------------------

  
  
  # Keep generalists too (14/10/2025 - I think I am removing them...30/10 removed)
  family_data <- family_data[family_data$tax.check1 %in% "high", ]

  # Get only "trees" lifeForm
  family_data <- dplyr::left_join(family_data, 
                                  our_names[, c("our_accepted_scientificName",
                                                "lifeForm_aggregated")],
                                  by = c("scientificName" = "our_accepted_scientificName"))
  family_data <- family_data[grepl("tree", family_data$lifeForm_aggregated), ]
  
  ##### Removing unusable data #####
  # Geo check: keep only ok_, shore
  family_data <- family_data[grepl("^ok_|shore", 
                                   family_data$geo.check1), ]

  # Lock check: keep only ok_ or check_local.2municip.
  family_data <- family_data[grepl("^ok_|^check_local.2municip.$",
                                   family_data$loc.check1), ]
  
  # Keep registers at state-level if the state is smaller than 12,500 km²
  # The same rationale is automatically applied to countries...
  family_data <- dplyr::left_join(family_data, latamMap_agg_adm[, c("name", "bad_reso")],
                                  by = c("loc.correct1" = "name"))
  
  # Keep only NA and FALSE on bad_reso
  family_data <- family_data[family_data$bad_reso %in% c(FALSE, NA), ]
  
  # Coord check: keep only records with coordinates
  family_data <- family_data[grepl("coord_gazet|coord_original", family_data$origin.coord1), ]
  
  # Cult check: keep only non-cultivated registers (make this better before: taxonomic list)
  family_data <- family_data[!family_data$cult.check %in% c("cultivated", "prob_cultivated"), ]
  
  # Outliers and distribution check: remove bad_dist and out.check is TRUE
  # We only remove a register if it falls outside of its indicated botanical country
  # <AND> is a spatial outlier
  # Converting D² distances to probabilities using the Chi-square dist.
  # degrees of freedom == 2 since we have two-dimensions (lon/lat)
  df <- 2
  alpha <- 0.025 # using 2.5% here as p-value threshold
  critVal <- qchisq(p = 1 - alpha, df = df)
  family_data$chisq.out.robust <- family_data$maha.robust > critVal
  # Also dealing with introduced species now
  family_data <- family_data[(!family_data$dist.check %in% c("bad_dist", "introduced") &
                                # !family_data$out.check %in% TRUE &
                                # family_data$maha.classic <= 2.5 &
                                !family_data$chisq.out.robust %in% TRUE), ]
  
  # Cases where there is no possible indication of outlier (not enough unique points)
  # we filter the data using only the locations that are ok_dist
  family_data <- family_data[!(is.na(family_data$out.check) & 
                                 family_data$dist.check %in% "bad_dist"), ]
  
  # Convert coordinate character columns to numeric
  family_data$decimalLatitude.new1 <- as.numeric(family_data$decimalLatitude.new1)
  family_data$decimalLongitude.new1 <- as.numeric(family_data$decimalLongitude.new1)
  
  # Cross-check the register with original coordinates against the maps
  with_coord_orig <- which(family_data$origin.coord1 %in% "coord_original")
  # Convert to SpatVect
  family_data_coord_orig <- terra::vect(family_data[with_coord_orig, ],
                                   geom = c("decimalLongitude.new1",
                                            "decimalLatitude.new1"),
                                   crs = "epsg:4326")
  
  # Cross with Neotropical map and EFG map
  # Neotropical
  # Need to use a buffer to get points close to the border due to mismatch
  # between gazetteer coordinates and maps (brazil_para_belem falls in water)
  family_data_coord_orig$is_nt <- terra::is.related(x = family_data_coord_orig,
                                                    y = tropical_america,
                                                    "intersects")
  # EFGs
  family_data_coord_orig_efgs <- terra::intersect(x = family_data_coord_orig,
                                                  y = efgs)
  
  if(all(family_data_coord_orig$is_nt %in% FALSE)){
    next
  }
  if(dim(family_data_coord_orig_efgs)[1] == 0){
    next
  }
  
  # df for merging both of them
  family_data_coord_orig_df <- terra::as.data.frame(family_data_coord_orig, geom = "XY")
  family_data_coord_orig_efgs <- terra::as.data.frame(family_data_coord_orig_efgs, geom = "XY")
  
  # Duplicate cases of a point being at two efgs...minor shapefile issue here
  dup_ids <- family_data_coord_orig_efgs$ids[duplicated(family_data_coord_orig_efgs$ids) | 
                                               duplicated(family_data_coord_orig_efgs$ids, 
                                                          fromLast = TRUE)]
  # Removing the register from both EFGs to not randomly inflate
  family_data_coord_orig_efgs <- family_data_coord_orig_efgs[!family_data_coord_orig_efgs$ids %in% dup_ids, ]
  
  # merge and retain the NAs too
  result_df_orig <- dplyr::left_join(x = family_data_coord_orig_df,
                                     y = family_data_coord_orig_efgs[, c("ids", "our_efg")],
                                     by = "ids")
  
  # Now, simply merge coord_gazet with latamMap info
  with_coord_gazet <- which(family_data$origin.coord1 %in% "coord_gazet")
  result_df_gazet <- dplyr::left_join(x = family_data[with_coord_gazet, ],
                                      y = gaz_efg[, c("loc.correc", "our_efg")],
                                      by = c("loc.correct1" = "loc.correc"))
  result_df_gazet <- result_df_gazet[!duplicated(result_df_gazet$ids), ]
  # The same for Neotropics
  result_df_gazet <- dplyr::left_join(result_df_gazet,
                                      gaz_nt[, c("loc.correc", "is_nt")],
                                      by = c("loc.correct1" = "loc.correc"))
  result_df_gazet <- result_df_gazet[!duplicated(result_df_gazet$ids), ]
  
  # Merge back with original data:
  to_merge <- rbind.data.frame(result_df_orig[, c("ids", "our_efg", "is_nt")],
                               result_df_gazet[, c("ids", "our_efg", "is_nt")])
  family_data <- dplyr::left_join(family_data, to_merge, by = "ids")
  
  ##### RELEVANT: cases where we have a shapefile, we can assign an area for
  # each adm level and consider transition/predominant regarding Neotropics and
  # each biome
  #####
  # Is the realm transitional or predominant?
  # Transitional?
  latam_gaz_trans <- latam_gaz[grepl("^Transition", latam_gaz$Morrone_re_new), ]
  get_these <- which(family_data$loc.correct1 %in% latam_gaz_trans$loc_gaz)
  family_data$is_realm_transitional <- FALSE
  family_data$is_realm_transitional[get_these] <- TRUE
  # Predominant?
  latam_gaz_pred <- latam_gaz[grepl("predominant$", latam_gaz$Morrone_re_new), ]
  get_these <- which(family_data$loc.correct1 %in% latam_gaz_pred$loc_gaz)
  family_data$is_realm_predominant <- FALSE
  family_data$is_realm_predominant[get_these] <- TRUE
  
  ##### Doing the same for biomes #####
  latam_gaz_trans <- latam_gaz[grepl("^Transition", latam_gaz$our_efg_new), ]
  get_these <- which(family_data$loc.correct1 %in% latam_gaz_trans$loc_gaz)
  family_data$is_biome_transitional <- FALSE
  family_data$is_biome_transitional[get_these] <- TRUE
  # Predominant?
  latam_gaz_trans <- latam_gaz[grepl("predominant$", latam_gaz$our_efg_new), ]
  get_these <- which(family_data$loc.correct1 %in% latam_gaz_trans$loc_gaz)
  family_data$is_biome_predominant <- FALSE
  family_data$is_biome_predominant[get_these] <- TRUE
  
  ##### For still NA cases, get the info from gazetteer objects #####
  which_na <- which(is.na(family_data$our_efg))
  # Create a lookup vector from gaz (loc.correct -> our_efg)
  gaz_lookup <- setNames(gaz_efg$our_efg, gaz_efg$loc.correc)
  # Fill NAs by matching loc.correct values
  family_data$our_efg[which_na] <- gaz_lookup[family_data$loc.correct1[which_na]]
  
  # Do the same for the Neotropics gaz
  which_na <- which(is.na(family_data$is_nt))
  gaz_lookup <- setNames(gaz_nt$is_nt, gaz_nt$loc.correc)
  family_data$is_nt[which_na] <- gaz_lookup[family_data$loc.correct1[which_na]]
  
  # Remove non-Neotropical endemic species ----------------------------------
  
  # 90% rule for now
  tab <- table(family_data$scientificName, family_data$is_nt, useNA = 'ifany')
  percent_true <- tab[, "TRUE"] / rowSums(tab, na.rm = FALSE) * 100
  percent_true[is.na(percent_true)] <- 0
  true_percent <- percent_true

  # Get names where ≥ 90% occurrences are within the Neotropics
  keep_species <- names(true_percent)[true_percent >= 90]
  # family_data <- family_data[family_data$scientificName %in% keep_species, ]
  ##### Also remove occurrences outside of the Neotropics or keep them? #####
  ##### Ofc, remove AFTER classifying as Neotropical #####
  family_data$is_nt_endemic <- family_data$scientificName %in% names(true_percent)[true_percent >= 90]
  
  # Calculate endemism per EFG (if >= 90% occurrences inside a single EFG, then
  # the species is endemic to it)
  family_data$efg_endemic <- NA
  species_list <- sort(unique(family_data$scientificName))
  
  for (j in species_list) {

    species_data <- family_data[family_data$scientificName %in% j, ]
    efg_counts <- table(species_data$our_efg)
    max_percent <- max(efg_counts) / sum(efg_counts) * 100
    
    # If ≥ 90%, assign the dominant EFG
    if (max_percent >= 90) {
      dominant_efg <- names(efg_counts)[which.max(efg_counts)]
      family_data$efg_endemic[family_data$scientificName %in% j] <- dominant_efg
    }
  }
  
  # Drop ids column and save
  # family_data$ids <- NULL
  
  # Saving by family 
  plantR::saveData(family_data,
                   dir.name = i,
                   path = file_path2,
                   by = "family",
                   file.format = "rds",
                   compress = TRUE)
  
}

