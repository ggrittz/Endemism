
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
  all_fam[[i]] <- readRDS(family_path)
  
}

# Bind it all
# Make everything character
all_fam <- lapply(all_fam, function(x) {
  data.frame(lapply(x, as.character), stringsAsFactors = FALSE)
})
all_fam <- dplyr::bind_rows(all_fam)

# Now, for interpolations, keep only registers that are inside the Neotropics
all_fam <- all_fam[all_fam$is_nt %in% TRUE, ]

# Dealing with transition zones and predominance --------------------------
# How many registers have 'Transition' or 'predominant' in its name?
table(grepl("\\|", all_fam$our_efg) |
        grepl("predominant", all_fam$our_efg)) # 28259 / 1123931 (2%)

# Duplicate rows by our_efg and remove Transitions to avoid problems later
expanded_rows <- as.data.frame(tidyr::separate_rows(all_fam, our_efg, sep = "\\|"))
# Remove all transitions to avoid headaches
expanded_rows <- expanded_rows[!grepl("Transition", expanded_rows$our_efg), ]
expanded_rows <- expanded_rows[!grepl("Transition", expanded_rows$efg_endemic), ]

# For richness, consider 'predominant' to be the biome per se
expanded_rows$our_efg <- gsub(" predominant$", "", expanded_rows$our_efg)
# For endemics, remove predominant
expanded_rows <- expanded_rows[!grepl("predominant", expanded_rows$efg_endemic), ]

# Generate species lists per EFG
species_list <- lapply(split(expanded_rows, expanded_rows$our_efg), 
                       function(x) unique(x$scientificName))

# Generate endemics species lists per EFG
species_list_end <- lapply(split(expanded_rows, expanded_rows$efg_endemic), 
                           function(x) unique(x$scientificName))

# Save these
path_to_save <- here::here("data", "derived-data", "rds")
file1 <- "spp_per_efg.rds"
file2 <- "end_per_efg.rds"
saveRDS(species_list, file.path(path_to_save, file1))
saveRDS(species_list_end, file.path(path_to_save, file2))

# Saving the final binded df
saveRDS(all_fam, file.path("data", "derived-data", "rds", "final_df.rds"))
rm(list=ls())


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



