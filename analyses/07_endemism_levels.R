
# Measuring endemism levels -----------------------------------------------

##### Read data #####

# Loading our classified latamMap
file_path <- here::here("data", "derived-data", "maps")
file_path1 <- here::here("data", "derived-data", "Cleaned_occs")
file_path2 <- here::here("data", "derived-data", "Endemic_species")
file_name <- "latamMap_classified.gpkg"
latam_gaz <- terra::vect(file.path(file_path, file_name))

# Load equivalence table for administrative levels that exist in gazetteer
# This is going to be used for keeping records at X, Y, Z adm level
options(gargle_oauth_cache = ".secrets")
# googlesheets4::gs4_auth() # is this needed?
googlesheets4::gs4_auth(cache = ".secrets",
                        email = "ggrittz@usp.br",
                        scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")
# Download from Google Drive
link <- "https://docs.google.com/spreadsheets/d/"
link1 <- "1V42RsVpUQuiHbv-U6U_t--8mCAr7UOjQLpQ9Awq6PRI/edit?gid=0#gid=0"
eq_table <- as.data.frame(googlesheets4::read_sheet(paste0(link, link1), range = "A:G"))
eq_table <- eq_table[, c("country", "latamMap_GADM")]

# Fast preparing latam_gaz complete loc.correct1 (NAME_all) in the same way
# it is from loc.correct
latam_gaz <- terra::as.data.frame(latam_gaz, geom = "WKT")
latam_gaz$loc_gaz <- apply(latam_gaz[, 1:4], 1, function(x) {
  non_na <- x[!is.na(x)]
  paste(non_na, collapse = "_")
})

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

##### Just to get some values... #####
##### TOTAL VALUES OF FAMILIES, GENUS, SPECIES THAT OCCUR IN NEOTROPICS #####
##### not necessarily endemics!!! #####
 fam_list <- list()
 cols <- c("tax.check1", "geo.check1", "origin.coord1", "cult.check", 
           "country.new","dist.check", "out.check", "scientificName", "scientificNameAuthorship", 
           "family",
           "decimalLatitude.new1", "decimalLongitude.new1",
           # Below are additions to work with plantR::checkList
           "catalogNumber", "collectionCode.new", "recordedBy.new", "recordNumber.new",
           "year.new", "stateProvince.new", "municipality.new", "loc.correct",
           "dup.ID", "dup.prop", "typeStatus", "numTombo", "month", "day")
 
 for(i in family_names){
   cat("Loading", i, "to summarise here", "\n")
   gz_path <- file.path(file_path1, i, paste0(i, ".csv.gz"))
   if(!file.exists(gz_path)){
     next
   }
   # Load only the specified columns
   family_data <- data.table::fread(gz_path, select = cols)
   # Converting D² distances to probabilities using the Chi-square dist.
   # degrees of freedom == 2 since we have two-dimensions (lon/lat)
   df <- 2
   alpha <- 0.025 # using 2.5% here as p-value threshold
   critVal <- qchisq(p = 1 - alpha, df = df)
   family_data$chisq.out.robust <- family_data$maha.robust > critVal
   fam_list[[i]] <- family_data
 }
 fam_list <- dplyr::bind_rows(fam_list) # 66,412,445 non dup records
 fam_list <- fam_list[fam_list$tax.check %in% "high", ] 
 fam_list <- fam_list[grepl("^ok_|shore", fam_list$geo.check), ]
 big_countries <- c("brazil", "colombia", "peru", "bolivia", 
                    "venezuela", "argentina", "mexico", "chile") ##### add algo sobre esse corte #####
 fam_list <- fam_list[!(fam_list$country.new %in% big_countries &
                                grepl("^ok_country($|\\[|_)", fam_list$geo.check)), ]
 fam_list <- fam_list[grepl("coord_gazet|coord_original",fam_list$origin.coord), ]
 fam_list <- fam_list[!fam_list$cult.check %in% c("cultivated", "prob_cultivated"), ]
 fam_list <- fam_list[(!fam_list$dist.check %in% "bad_dist" &
                               !fam_list$chisq.out.robust %in% TRUE), ] # 1,819,151 records after all filters
 # fam_list <- fam_list[!(is.na(fam_list$out.check) & 
 #                          fam_list$dist.check %in% "bad_dist"), ]
 
 # Get life form classification from our list
 fam_list <- dplyr::left_join(fam_list, our_names[, c("our_accepted_scientificName", 
                                                       "lifeForm_aggregated")],
                              by = c("scientificName" = "our_accepted_scientificName"))
 fam_list$decimalLatitude.new <- as.numeric(fam_list$decimalLatitude.new)
 fam_list$decimalLongitude.new <- as.numeric(fam_list$decimalLongitude.new)
 fam_list <- terra::vect(fam_list, geom = c("decimalLongitude.new",
                                            "decimalLatitude.new"),
                         crs = "epsg:4326")
 fam_list$is_nt <- terra::is.related(x = fam_list,
                                     y = tropical_america,
                                     "intersects")
 
 ##### How many families? #####
 length(unique(fam_list$family[fam_list$is_nt %in% TRUE & # 241 for shrubs
                                 grepl("tree", 
                                       fam_list$lifeForm_aggregated)])) # 216 families
 # How many genus?
 length(unique(sapply(strsplit(fam_list$scientificName[fam_list$is_nt %in% TRUE &
                                                         grepl("tree", 
                                                               fam_list$lifeForm_aggregated)], " "), 
                      function(x) x[1]))) # 2,088 genus # 2,855 for shrubs
 
 # How many species?
 length(unique(fam_list$scientificName[fam_list$is_nt %in% TRUE &  # 27,025 in Neotropics 42,558 for shrubs
                                         grepl("tree", 
                                               fam_list$lifeForm_aggregated)]))
 

 
 fam_list$decimalLatitude.new <- as.numeric(fam_list$decimalLatitude.new)
 fam_list$decimalLongitude.new <- as.numeric(fam_list$decimalLongitude.new)
 fam_list <- terra::vect(fam_list, geom = c("decimalLongitude.new",
                                            "decimalLatitude.new"),
                         crs = "epsg:4326")
 fam_list$is_nt <- terra::is.related(x = fam_list,
                                     y = tropical_america,
                                                   "intersects")

#



# Get a Realm/EFG for each occurrence -------------------------------------

# Do it by family
for(i in family_names){
  
  cat("Calculating endemism levels for", i, "\n")
  gz_path <- file.path(file_path1, i, paste0(i, ".csv.gz"))
  
  if(!file.exists(gz_path)){
    cat("Input file not found, skipping \n")
    next
  }
  
  family_data <- data.table::fread(gz_path)
  
  # family_data <- Lauraceae
  family_data$ids <- 1:nrow(family_data)
  
  # REMOVING THIS STEP FOR NOW #
  ##### Keep only records validated by a taxonomists ($tax.check == 'high') #####
  # high_names <- unique(all_fam$scientificName[all_fam$tax.check %in% "high"])
  # family_data <- family_data[family_data$tax.check %in% c("high", "medium"), ]
  
  # Get only "trees" lifeForm
  family_data <- dplyr::left_join(family_data, 
                                  our_names[, c("our_accepted_scientificName",
                                                "lifeForm_aggregated")],
                                  by = c("scientificName" = "our_accepted_scientificName"))
  family_data <- family_data[grepl("tree", family_data$lifeForm_aggregated), ]
  
  ##### Removing unusable data #####
  # Geo check: keep only ok_ and shore (this could be just some mapping issues)
  family_data <- family_data[grepl("^ok_|shore", family_data$geo.check1), ]

  # Keep only records that are at the minimal administrative level each LatAm country
  # have in our gazetteer. For cases where the minimum ADM level is below municipality,
  # we keep everything at municipality (nothing above)
  # Example: for Bahamas, our gazetteer only have information at ADM ("state"-level)
  family_data <- dplyr::left_join(family_data, eq_table, by = c("country.new" = "country"))
  family_data$to_use_geo <- FALSE
  
  # For ADM1: tag TRUE if geo.check1 starts with "ok_state", "ok_county", or "ok_locality"
  family_data$to_use_geo[family_data$latamMap_GADM %in% "ADM1" & 
                           grepl("^ok_state|^ok_county|^ok_locality", 
                                 family_data$geo.check1,)] <- TRUE
  
  # For ADM2: tag TRUE if geo.check1 starts with "ok_county" or "ok_locality"
  family_data$to_use_geo[family_data$latamMap_GADM %in% "ADM2" & 
                           grepl("^ok_county|^ok_locality", 
                                 family_data$geo.check1)] <- TRUE
  
  # For ADM3: tag TRUE if geo.check1 starts with "ok_county" or "ok_locality"
  family_data$to_use_geo[family_data$latamMap_GADM %in% "ADM3" & 
                           grepl("^ok_county|^ok_locality", 
                                 family_data$geo.check1)] <- TRUE
  
  # For Uruguay specifically (they only have ADM 1): use ADM 1 or below
  family_data$to_use_geo[family_data$country.new %in% "uruguay" & 
                           grepl("^ok_state|^ok_county|^ok_locality", 
                                 family_data$geo.check1)] <- TRUE
  
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
  family_data <- family_data[(!family_data$dist.check %in% "bad_dist" &
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
                                            "decimalLatitude.new1"))
  terra::crs(family_data_coord_orig) <- "epsg:4326"
  
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
                                      y = latam_gaz[, c("loc_gaz", "our_efg_new")],
                                      by = c("loc.correct" = "loc_gaz"))
  names(result_df_gazet)[which(names(result_df_gazet) == "our_efg_new")] <- "our_efg"
  
  # Merge back with original data:
  to_merge <- rbind.data.frame(result_df_orig[, c("ids", "our_efg")],
                               result_df_gazet[, c("ids", "our_efg")])
  # Get the is_nt column from family_data_coord_orig
  to_merge <- dplyr::left_join(to_merge, family_data_coord_orig_df[, c("ids", "is_nt")],
                               by = "ids")
  family_data <- dplyr::left_join(family_data, to_merge, by = "ids")
  
  
  ##### For still NA cases, get the info from gazetteer objects #####
  which_na <- which(is.na(family_data$our_efg))
  # Create a lookup vector from gaz (loc.correct -> our_efg)
  gaz_lookup <- setNames(gaz_efg$our_efg, gaz_efg$loc.correct)
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
  family_data <- family_data[family_data$scientificName %in% keep_species, ]
  ##### Also remove occurrences outside of the Neotropics of keep them? #####
  ##### Ofc, remove AFTER classifying as Neotropical #####
  
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
  family_data$ids <- NULL
  
  # Saving by family 
  plantR::saveData(family_data,
                   dir.name = i,
                   path = file_path2,
                   by = "family",
                   file.format = "rds",
                   compress = TRUE)
  
}

