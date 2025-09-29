
# Calculate endemism ------------------------------------------------------
devtools::load_all()

# Load data
file_path <- here::here("data", "derived-data", "Endemic_species")
family_names <- readRDS(here::here("data", "derived-data", "family_names.rds"))

# Region maps
tropical_america <- here::here("data", "derived-data", "maps", "NTAmericas_dissolved.shp")
tropical_america <- terra::vect(tropical_america, crs = "epsg:4326")
tropical_america <- tropical_america[tropical_america$Morrone_re %in% "Neotropical", ]
efg <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp"))
efg <- efg[efg$Morrone_re %in% "Neotropical", ]

# Load tree species names
our_names <- readRDS(here::here("data", "derived-data", "checklist_standardized.rds"))
# Get unique our_accepted_scientificNames
our_names1 <- our_names[!duplicated(our_names$our_accepted_scientificName), ]

# Load necessary info by family
all_fam <- list()

for(i in family_names){
  
  cat("Loading", i, "\n")
  family_path <- file.path(file_path, i, paste0(i, ".rds"))
  
  if(!file.exists(family_path)){
    cat("Input file not found, skipping \n")
    next
  }

  # Keeping all columns. They were filtered before anyway
  all_fam[[i]] <- readRDS(family_path)  # [, c("collectionCode.new", "recordedBy.new",
                                        #     "identifiedBy.new", "recordNumber.new",
                                        #     "year.new", "yearIdentified.new",
                                        #     "country.new", "stateProvince.new",
                                        #     "municipality.new", "locality.new",
                                        #     "loc.correct1", "decimalLongitude.new", 
                                        #     "decimalLatitude.new", "scientificName", 
                                        #     "family", "our_efg", "is_nt", "efg_endemic",
                                        #      "identifiedBy.new", "catalogNumber",
                                        #     "geo.check1", "tax.check1")]
}

# Bind it all
all_fam <- dplyr::bind_rows(all_fam)

##### Before calculating the metrics, there are a few minor manipulations needed #####
# Keep only records validated by a taxonomists ($tax.check == 'high')
# high_names <- unique(all_fam$scientificName[all_fam$tax.check %in% "high"])
all_fam1 <- all_fam[all_fam$tax.check %in% "high", ]

# Get only "trees" lifeForm
##### Should I add the source here too? #####
all_fam1 <- dplyr::left_join(all_fam1, our_names1[, c("our_accepted_scientificName", 
                                                     "lifeForm_aggregated")],
                             by = c("scientificName" = "our_accepted_scientificName"))
all_fam1 <- all_fam1[grepl("tree", all_fam1$lifeForm_aggregated), ]

# Dealing with transition zones and predominance --------------------------
# How many registers have 'Transition' or 'predominant' in its name?
table(grepl("\\|", all_fam1$our_efg) |
        grepl("predominant", all_fam1$our_efg)) # 28259 / 1123931 (2%)

##### How many families, genus, and species are endemic? #####
length(unique(all_fam1$family)) # 206 families
length(unique(sapply(strsplit(all_fam1$scientificName, " "), 
                     function(x) x[1]))) # 1,954 genus
length(unique(all_fam1$scientificName)) # 24,693 species/subspecies/varieties

# For richness, split 'Transition EFGs' to count the register for all efgs it transitions
expanded_rows <- as.data.frame(tidyr::separate_rows(all_fam1, our_efg, sep = "\\|"))
# tidyr is extremely much faster
# expanded_rows <- lapply(seq_len(nrow(all_fam1)), function(i) {
#   efg_split <- unlist(strsplit(all_fam1$our_efg[i], "\\|"))
#   data.frame(scientificName = rep(all_fam1$scientificName[i], length(efg_split)),
#              our_efg = efg_split)
# })
# expanded_rows <- dplyr::bind_rows(expanded_rows)
expanded_rows$our_efg <- gsub("Transition ", "", expanded_rows$our_efg)
# Keep only unique cases
expanded_df <- expanded_rows[!duplicated(expanded_rows), ]
# Merge back
# expanded_df <- plyr::rbind.fill(all_fam1[!composite_efg, ], expanded_rows)
# For richness, we are considering predominant to be the indicated EFG
expanded_df$our_efg <- gsub(" predominant", "", expanded_df$our_efg)
# expanded_df$efg_endemic <- gsub(" predominant", "", expanded_df$efg_endemic)

# Now, keep only Neotropical endemics here
expanded_df <- expanded_df[expanded_df$is_nt %in% TRUE, ]

##### Richness per EFG #####
richness <- aggregate(scientificName ~ our_efg, 
                      data = expanded_df, 
                      FUN = function(x) length(unique(x)))

##### Endemics per EFG #####
# For endemics, do not count transitions nor predominance as is; keep separated
endemic_species <- unique(expanded_df[!is.na(expanded_df$efg_endemic), 
                                      c("efg_endemic", "scientificName")])
endemic_count <- as.data.frame(table(endemic_species$efg_endemic))
names(endemic_count) <- c("our_efg", "endemics")

# Merging
result <- dplyr::left_join(richness, endemic_count, by = "our_efg")
names(result) <- c("EFG", "Richness", "Endemics")
result$Endemism_level <- round(100 * (result$Endemics / result$Richness), 2)

# Get efg area
result <- dplyr::left_join(result, 
                           terra::as.data.frame(efg[, c("our_efg", "er_Area_km")], geom = NULL),
                           by = c("EFG" = "our_efg"))
names(result)[which(names(result) == "er_Area_km")] <- "EFG_area_km"

if(!dir.exists(here::here("outputs", "tables")))
  dir.create(here::here("outputs", "tables"))
write.csv(result, here::here("outputs", "tables", "efg_summary_table.csv"))

# Generate species lists per EFG
species_list <- lapply(split(expanded_df, expanded_df$our_efg), 
                       function(x) unique(x$scientificName))

# Generate endemics species lists per EFG
species_list_end <- lapply(split(expanded_df, expanded_df$efg_endemic), 
                           function(x) unique(x$scientificName))

# Save these
path_to_save <- here::here("data", "derived-data", "rds")
file1 <- "spp_per_efg.rds"
file2 <- "end_per_efg.rds"
saveRDS(species_list, file.path(path_to_save, file1))
saveRDS(species_list_end, file.path(path_to_save, file2))


# What names are missing from our registers? (only good taxonomic sources here)
##### Or keep all sources anyway? #####
all_trees <- our_names1$our_accepted_scientificName[
  grepl("tree",our_names1$lifeForm_aggregated)]

##### To do this properly, we may need another filter...these are Americas' names, not #####
##### only tropical America... #####
# These registers occur in the Americas but we didn't found a good taxonomist
# to vouch for them
missing_trees <- setdiff(all_trees, unique(all_fam1$scientificName))
saveRDS(missing_trees, file.path("data", "derived-data", "rds", "missing_trees.rds"))

# Saving the final df
all_fam2 <- all_fam1[all_fam1$is_nt %in% TRUE, ] # 1,142,761 registers
saveRDS(all_fam2, file.path("data", "derived-data", "rds", "final_df.rds"))


# # Save all_fam2 df in the NDM/VNDM format for posterior analyses
# 
# ##### Prepare final df for endemicity analysis in VNDM #####
# all_fam2 <- readRDS(file.path("data", "derived-data", "rds", "final_df.rds"))
# 
# # Append this map later in the end of the .xyd file
# tropical_america <- terra::vect(file.path("data", "derived-data", "maps", "NTAmericas_dissolved.shp"))
# tropical_america <- tropical_america[tropical_america$Morrone_re %in% "Neotropical", ]
# tropical_america1 <- terra::simplifyGeom(tropical_america, tolerance = 0.25)
# tropical_america2 <- terra::geom(tropical_america1, df = TRUE)
# 
# # Only necessary columns
# for_ndm <- all_fam2[, c("scientificName", "decimalLongitude.new", "decimalLatitude.new")]
# names(for_ndm) <- c("NAME1", "XCOOR", "YCOOR")
# for_ndm$NAME1 <- gsub("\\[", "\\(", for_ndm$NAME1)
# for_ndm$NAME1 <- gsub("\\]", "\\)", for_ndm$NAME1)
# 
# # Internal function
# ndm_xyd(for_ndm)
# 
# # Append the map vertices to this same file
# cat("map" , file = "./data/derived-data/NDM_DB.xyd", fill = TRUE, append = TRUE)
# cat("line" , file = "./data/derived-data/NDM_DB.xyd", fill = TRUE, append = TRUE)
# full <- do.call(paste, c(tropical_america2[, 4:3], sep = " ", collapse = "\n"))
# cat(full, 
#     file = "./data/derived-data/NDM_DB.xyd", fill = TRUE, append = TRUE)




##### Finally, calculating weighted endemism #####

# Weighted Endemism -------------------------------------------------------

efg <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp"))
efg <- efg[efg$Morrone_re %in% "Neotropical", ]
final_df <- readRDS(here::here("data", "derived-data", "rds", "final_df.rds"))
efg_end <- readRDS(here::here("data", "derived-data", "rds", "end_per_efg.rds"))

# Do not calculate WEE on transitionals
efg_end <- efg_end[!grepl("Transition|predominant", names(efg_end))]

# only 15 remanining: three have no endemics and one have no records
efg_names <- names(efg_end)

# svg("figures/FigureSX.3.svg", width = 10, height = 10)
# par(mfrow = c(3, 2))
# for(i in seq_along(efg_names[11:15])){
#   terra::plot(efg[efg$our_efg %in% efg_names[i], ], main = efg_names[i])
# }
# dev.off()

# Grid cell resolution to be used
base_res <- 0.5 # 111.32 km = 1 grid degree at Equator

# Create template raster for calculating WEE in ALL tropical America
r_template <- terra::rast(extent = terra::ext(efg),
                          resolution = base_res,
                          crs = "epsg:4326")
# Rasterize the Tropical America map into the template
raster_output <- terra::rasterize(efg, r_template, field = "our_efg")
# Convert to {raster} since the Guerin 2015 function was made on it
# Check for our own updates if time makes it possible #
raster_raster <- raster::raster(raster_output)


# Do the same but now for each biome separately
rasterized_biomes <- list()

for(i in seq_along(efg_names)){
  # Subset the SpatVector for the current biome
  efg_subset <- efg[efg$our_efg == efg_names[i], ]
  
  # Create template for this biome
  biome_template <- terra::rast(extent = terra::ext(r_template),
                                resolution = base_res,
                                crs = "epsg:4326")
  
  # Rasterize this specific biome
  rasterized_biomes[[i]] <- terra::rasterize(efg_subset, biome_template, field = "our_efg")
}

# Name the list elements for easier reference
names(rasterized_biomes) <- efg_names
# They need to be {raster} format
rasterized_biomes <- lapply(rasterized_biomes, raster::raster)


# Mapping weighted endemism using 'area' (EOO convex hull per se) ---------

##### All tropical America #####
we_map_area = weighted.endemism(species_records = final_df,
                                species = "scientificName",
                                latitude = "decimalLatitude.new",
                                longitude = "decimalLongitude.new",
                                frame.raster = raster_raster,
                                outlier_pct = 99,
                                weight.type = "geo",
                                geo.type = "cell",
                                geo.calc = "polygon",
                                plot.raster = FALSE)

# Saving
saveRDS(we_map_area, here::here("data", "derived-data", "rds", "we_map_area_50km.rds"))

##### All biomes now, but separately #####
# To save
we_map_area_biomes <- list()

for(i in seq_along(efg_names)){
  
  # Subset each group of single-biome endemics
  data_to_use <- final_df[final_df$scientificName %in% efg_end[[i]], ]
  # Convert to terra and mask the points inside the specific biome-only
  # This avoids duplicating cell names for later usage
  data_to_use <- terra::vect(data_to_use, 
                             geom = c("decimalLongitude.new", "decimalLatitude.new"),
                             crs = "epsg:4326")
  biome_polygon <- terra::as.polygons(terra::rast(rasterized_biomes[[i]]))
  data_to_use <- terra::intersect(data_to_use, biome_polygon)
  
  # Back to data.frame
  data_to_use <- terra::as.data.frame(data_to_use, geom = "XY")
  
  we_map_area = weighted.endemism(species_records = data_to_use,
                                  species = "scientificName",
                                  latitude = "y",
                                  longitude = "x",
                                  frame.raster = rasterized_biomes[[i]],
                                  outlier_pct = 99,
                                  weight.type = "geo",
                                  geo.type = "cell",
                                  geo.calc = "polygon",
                                  plot.raster = FALSE)
  
  we_map_area_biomes[[i]] <- we_map_area
  
}
names(we_map_area_biomes) <- efg_names

# Saving
saveRDS(we_map_area_biomes, here::here("data", "derived-data", "rds", "we_map_area_biomes_50km.rds"))

# rm(list=ls())



