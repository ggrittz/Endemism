
# Measuring endemism levels -----------------------------------------------

##### Read data #####

# Loading our classified latamMap
file_path <- here::here("data", "derived-data", "maps")
file_path1 <- here::here("data", "derived-data", "Cleaned_occs")
file_name <- "latamMap_classified.gpkg"
latam_gaz <- terra::vect(file.path(file_path, file_name))

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

# Loading family names
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))


# Get a Realm/EFG for each occurrence -------------------------------------

# Do it by family
for(i in family_names){
  
  cat("Reading", i, "\n")
  gz_path <- file.path(file_path1, i, paste0(i, ".csv.gz"))
  
  if(!file.exists(gz_path)){
    cat("Input file not found, skipping \n")
    next
  }
  
  family_data <- data.table::fread(gz_path)
  family_data$ids <- 1:nrow(family_data)
  
  # Convert coordinate character columns to numeric
  family_data$decimalLatitude.new1 <- as.numeric(family_data$decimalLatitude.new1)
  family_data$decimalLongitude.new1 <- as.numeric(family_data$decimalLongitude.new1)
  
  # Cross-check the register with original coordinates against the maps
  with_coord_orig <- which(family_data$origin.coord1 %in% "coord_original")
  # Convert to SpatVect
  family_data_coord <- terra::vect(family_data[with_coord_orig, ],
                                   geom = c("decimalLongitude.new1",
                                            "decimalLatitude.new1"))
  terra::crs(family_data_coord) <- "epsg:4326"
  
  # Make a ID row for each register to merge back later since terra functions
  # only return values for a case where a intersection exists
  family_data_coord$ids <- 1:nrow(family_data_coord)
  
  # Cross with Neotropical map and EFG map
  # Neotropical
  family_data_coord$is_nt <- terra::is.related(x = family_data_coord,
                                               y = tropical_america,
                                               "intersects")
  # EFGs
  family_data_coord_efgs <- terra::intersect(x = family_data_coord,
                                             y = efgs)
  
  # df for merging both of them
  family_data_coord_df <- terra::as.data.frame(family_data_coord, geom = "XY")
  family_data_coord_efgs <- terra::as.data.frame(family_data_coord_efgs, geom = "XY")
  
  # merge and retain the NAs too
  result_df_orig <- dplyr::left_join(x = family_data_coord_df,
                                y = family_data_coord_efgs[, c("ids", "Morrone_re", "our_efg")],
                                by = "ids")
  
  
  # Now, simply merge coord gazet with latamMap info
  with_coord_gazet <- which(family_data$origin.coord1 %in% "coord_gazet")
  result_df_gazet <- dplyr::left_join(x = family_data[with_coord_gazet, ],
                                      y = latam_gaz[, c("loc_gaz", "our_efg_new", "Morrone_re_new")],
                                      by = c("loc.correct1" = "loc_gaz"))
  
  
  ##### códigos +- prontos acima #####
  
  
  
  
  # Drop ids column and save
  result_df$ids <- NULL
  
  
}








