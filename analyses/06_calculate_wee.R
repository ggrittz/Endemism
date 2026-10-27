
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

# Load necessary info by family
all_fam <- list()

for(i in family_names){
  
  cat("Loading", i, "\n")
  family_path <- file.path(file_path, i, paste0(i, ".rds"))
  
  if(!file.exists(family_path)){
    cat("Input file not found, skipping \n")
    next
  }

  family_data <- readRDS(family_path)
  # Convert everything to character
  family_data[] <- lapply(family_data, as.character)
  # Keeping all columns. They were filtered before anyway
  all_fam[[i]] <- family_data
  
}

# Bind it all
all_fam <- dplyr::bind_rows(all_fam)


# Hope they do not exist anymore...
# get_these <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", all_fam$taxonName, perl = TRUE))
# Only eleven registers! Nice
# How about NA?
# sum(is.na(all_fam$taxonName)) no NA

# Save this unfiltered df
saveRDS(all_fam, file.path("data", "derived-data", "rds", "final_df_unfiltered.rds"))

# Numbers for the manuscript (spatial dataset) -------------------------------
bor <- 100 * prop.table(table(toupper(all_fam$basisOfRecord)))
dir.create(here::here("outputs", "numbers"), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(data.frame(key = c("n_spp_spatial", paste0("pct_bor_", tolower(names(bor)))),
                            value = c(length(unique(all_fam$scientificName)), as.numeric(bor)),
                            description = c("Species in the spatial dataset",
                                            paste("% of spatial records with basisOfRecord", names(bor)))),
                 here::here("outputs", "numbers", "07_calculate_wee_spatial.csv"), row.names = FALSE)
rm(bor)

# Now, for interpolations, keep only registers that are inside the Neotropics
all_fam <- all_fam[all_fam$is_nt %in% TRUE, ]


# Removing species introduced in tropical America -------------------------
# This will take care of cases such as Carica papaya L., which occurs in Brazil
# but its distribution is only on Central America
all_fam <- all_fam[!all_fam$dist.check %in% "introduced", ]

# Remove these ones too
exotics <- paste(c("Mangifera", "Cocos", "Artocarpus", "Hovenia", "Eriobotrya",
                   "Eucalyptus", "Grevillea", "Ligustrum", "Pittosporum", "Syzygium",
                   "Spathodea", "Citrus", "Coffea", "Cupressus", "Thespesia", "Delonix",
                   "Terminalia catappa", "Morus nigra", "Melia azedarach", "Azadirachta indica",
                   "Dracaena fragrans", "Albizia procera", "Albizia lebbeck",
                   "Ptychosperma propinquum"), collapse = "|")
all_fam <- all_fam[!grepl(exotics, all_fam$taxonName), ]

# Keep only Latin America/Caribbean countries
all_fam$geo.reg <- countrycode::countrycode(all_fam$country.new,
                                            origin = "country.name",
                                            destination = "region")

# Filtering only tropical American registers
all_fam <- all_fam[all_fam$geo.reg %in% c("Latin America & Caribbean"), ]


# Dealing with transition zones and predominance --------------------------

##### Decision 07/10/2025: for global interpolation: only Nt records
##### Decision 07/10/2025: for biome interpolation: only biome endemics

# # Duplicate rows by our_efg and remove Transitions to avoid problems later
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
# Keeping only non-predominant non-transitional cases here
all_fam_endemics <- all_fam[all_fam$is_biome_predominant %in% FALSE
                           &
                             all_fam$is_biome_transitional %in% FALSE
                           &
                             all_fam$is_realm_predominant %in% FALSE
                           & 
                             all_fam$is_realm_transitional %in% FALSE, ]

species_list_end <- lapply(split(all_fam_endemics, all_fam_endemics$efg_endemic),
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


# Weighted Endemism -------------------------------------------------------
efg <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp"))
efg <- efg[efg$Morrone_re %in% "Neotropical", ]
final_df <- readRDS(here::here("data", "derived-data", "rds", "final_df.rds"))
final_df$decimalLatitude.new1 <- as.numeric(final_df$decimalLatitude.new1)
final_df$decimalLongitude.new1 <- as.numeric(final_df$decimalLongitude.new1)
efg_end <- readRDS(here::here("data", "derived-data", "rds", "end_per_efg.rds"))

# Do not calculate WEE on transitionals
# efg_end <- efg_end[!grepl("Transition|predominant", names(efg_end))]

# only 15 remanining: three have no endemics and one have no records
efg_names <- names(efg_end)

# svg("figures/FigureSX.3.svg", width = 10, height = 10)
# par(mfrow = c(3, 2))
# for(i in seq_along(efg_names[11:15])){
#   terra::plot(efg[efg$our_efg %in% efg_names[i], ], main = efg_names[i])
# }
# dev.off()


# Deciding grid cell resolution to be used --------------------------------
# a = final_df[, c("decimalLongitude.new1", "decimalLatitude.new1")]
# 
# a_sf <- sf::st_as_sf(a, coords = c("decimalLongitude.new1", 
#                                    "decimalLatitude.new1"), crs = 4326)
# 
# cell_sizes <- c(0.25, 0.5, 1)
# 
# results <- lapply(cell_sizes, function(res) {
# 
#     grd <- sf::st_make_grid(tropical_america, cellsize = res, square = TRUE)
#   grd_sf <- sf::st_sf(cell_id = seq_along(grd), geometry = grd)
#   
#   joined <- sf::st_join(a_sf, grd_sf, join = sf::st_within)
#   counts <- table(joined$cell_id, useNA = "no")
#   
#   data.frame(
#     res              = res,
#     n_cells_total    = length(grd),
#     n_cells_occupied = length(counts),
#     pct_occupied     = round(100 * length(counts) / length(grd), 2),
#     n_points_unassigned = sum(is.na(joined$cell_id))
#   )
# })
# 
# do.call(rbind, results) 0.5 it is. 20% of the cells are occupied

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
we_map_area = weighted.endemism(species_records = final_df[final_df$is_realm_predominant %in% FALSE
                                                           &
                                                             final_df$is_realm_transitional %in% FALSE
                                                           & !is.na(final_df$decimalLongitude.new1)
                                                           & !is.na(final_df$decimalLatitude.new1), ],
                                species = "scientificName",
                                latitude = "decimalLatitude.new1",
                                longitude = "decimalLongitude.new1",
                                frame.raster = raster_raster,
                                outlier_pct = 99,
                                weight.type = "geo",
                                geo.type = "cell",
                                geo.calc = "polygon",
                                plot.raster = FALSE)

# Saving
saveRDS(we_map_area, here::here("data", "derived-data", "rds", "we_map_area_50km.rds"))

# Numbers for the manuscript (records and species used in the WE map) -------
we_input <- final_df[final_df$is_realm_predominant %in% FALSE
                     & final_df$is_realm_transitional %in% FALSE
                     & !is.na(final_df$decimalLongitude.new1)
                     & !is.na(final_df$decimalLatitude.new1), ]
utils::write.csv(data.frame(key = c("n_records_endemism_def", "n_records_we", "n_spp_we"),
                            value = c(nrow(final_df), nrow(we_input), length(unique(we_input$scientificName))),
                            description = c("Records left after applying the endemism definition (final_df)",
                                            "Records used in the WE map of tropical America",
                                            "Species endemic to tropical America used in the WE map")),
                 here::here("outputs", "numbers", "07_calculate_wee_we.csv"), row.names = FALSE)
rm(we_input)



##### All biomes now, but separately #####
# To save

# Calculating only to the top biomes we are going to show in the paper
usable_biomes <- c("Pyric tussock savannas",
                   "Tropical-subtropical dry forests and thickets",
                   "Tropical-subtropical montane rainforests",
                   "Tropical-subtropical lowland rainforests")

efg_end <- efg_end[names(efg_end) %in% usable_biomes]
rasterized_biomes <- rasterized_biomes[names(rasterized_biomes) %in% usable_biomes]
efg_names <- efg_names[efg_names %in% names(efg_end)]

we_map_area_biomes <- list()
for(i in seq_along(efg_names)){
  
  # Subset each group of single-biome endemics
  data_to_use <- final_df[final_df$scientificName %in% efg_end[[i]], ]
  data_to_use$decimalLongitude.new1 <- as.numeric(data_to_use$decimalLongitude.new1)
  data_to_use$decimalLatitude.new1 <- as.numeric(data_to_use$decimalLatitude.new1)
  # Convert to terra and mask the points inside the specific biome-only
  # This avoids duplicating cell names for later usage
  data_to_use <- terra::vect(data_to_use, 
                             geom = c("decimalLongitude.new1", "decimalLatitude.new1"),
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
                                  plot.raster = FALSE,
                                  verbose = FALSE)
  
  we_map_area_biomes[[i]] <- we_map_area
  
}
names(we_map_area_biomes) <- efg_names

# Saving
saveRDS(we_map_area_biomes, here::here("data", "derived-data", "rds", "we_map_area_biomes_50km.rds"))


# Calculate the null expectation ------------------------------------------
we_map_area <- readRDS(here::here("data", "derived-data", "rds", "we_map_area_50km.rds"))
we_map_area_biomes <- readRDS(here::here("data", "derived-data", "rds", "we_map_area_biomes_50km.rds"))

# Tropical America as a whole
set.seed(2026)
we_null_test <- endemism.null.test(we_map_area, nrep = 999)
# Each biome separately
we_null_test_biomes <- lapply(we_map_area_biomes, function(x) {
  endemism.null.test(x, nrep = 999)
})
names(we_null_test_biomes) <- names(we_map_area_biomes)

# Save
save_path <- here::here("data", "derived-data", "rds")
save_name1 <- "we_null_TA_all.rds"
save_name2 <- "we_null_TA_biomes.rds"
saveRDS(we_null_test, file.path(save_path, save_name1))
saveRDS(we_null_test_biomes, file.path(save_path, save_name2))

#
