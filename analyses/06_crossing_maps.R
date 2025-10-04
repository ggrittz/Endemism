
# Crossing areas with plantR map ------------------------------------------

# Loading data
latamMap <- plantR::latamMap
latamMap <- dplyr::bind_rows(latamMap)
latamMap <- terra::vect(latamMap)
# To compare later
latamMap$NAME_all <- paste(latamMap$NAME_0,
                           latamMap$NAME_1,
                           latamMap$NAME_2,
                           latamMap$NAME_3,
                           1:dim(latamMap)[1],
                           sep = "_")

# Do the same for the gazetteers
gaz <- plantR:::gazetteer
gaz <- terra::vect(gaz, geom = c("longitude.gazetteer", "latitude.gazetteer"),
                   crs = "epsg:4326")

# ##### Already worked with #####
# nt_dis <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_dissolved.shp"))
# nt_efg <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp"))
# 
# # Converting to sf to intersect (TopologyException error in terra)
# latamMap <- terra::simplifyGeom(latamMap, tolerance = 0.00001)
# nt_dis <- terra::simplifyGeom(nt_dis, tolerance = 0.00001)
# 
# # Intersect against the Palearctic+Neotropical+Andean map
# latamMap_nt <- terra::intersect(latamMap, nt_dis)
# latamMap_nt$Morrone_re <- gsub("Neotropical_temperate", "Neotropical temperate",
#                            latamMap_nt$Morrone_re)
# # Do the same for gaz
# gaz_nt <- terra::intersect(gaz, nt_dis)
# gaz_nt$Morrone_re <- gsub("Neotropical_temperate", "Neotropical temperate",
#                          gaz_nt$Morrone_re)
# 
# # Intersect against the EFGs
# latamMap_efg <- terra::intersect(latamMap, nt_efg)
# latamMap_efg$Morrone_re <- gsub("Neotropical_temperate", "Neotropical temperate",
#                             latamMap_efg$Morrone_re)
# # Do the same for gaz
# gaz_efg <- terra::intersect(gaz, nt_efg)
# gaz_efg$Morrone_re <- gsub("Neotropical_temperate", "Neotropical temperate",
#                           gaz_efg$Morrone_re)
# 
# # Save these maps since it takes some time to run this intersection
# file_path <- here::here("data", "derived-data", "maps")
# file_name <- "latamMap_intersected_nt.shp"
# file_name1 <- "latamMap_intersected_efg.shp"
# file_name2 <- "gaz_intersected_nt.shp"
# file_name3 <- "gaz_intersected_efg.shp"
# terra::writeVector(latamMap_nt, file.path(file_path, file_name), overwrite = TRUE)
# terra::writeVector(latamMap_efg, file.path(file_path, file_name1), overwrite = TRUE)
# terra::writeVector(gaz_nt, file.path(file_path, file_name2), overwrite = TRUE)
# terra::writeVector(gaz_efg, file.path(file_path, file_name3), overwrite = TRUE)




# Read here the maps saved above
file_path <- here::here("data", "derived-data", "maps")
file_name1 <- "latamMap_intersected_efg.shp"
file_name <- "latamMap_intersected_nt.shp"
latamMap_nt <- terra::vect(file.path(file_path, file_name))
latamMap_efg <- terra::vect(file.path(file_path, file_name1))

# Make sense CRS in all three maps
latamMap <- terra::project(latamMap, "epsg:4326")
latamMap_efg <- terra::project(latamMap_efg, "epsg:4326")
latamMap_nt <- terra::project(latamMap_nt, "epsg:4326")

# What cases occur in more than one realm?
# First, get duplicates from NAME_all
dups <- latamMap_nt[duplicated(latamMap_nt$NAME_all) | 
                       duplicated(latamMap_nt$NAME_all, fromLast = TRUE), ]

# Then, get duplicates that do have more than a single class of realm
# There are cases like Florianópolis where the island and the continental are
# different shapefiles, but both have the same name and occur in the same realm...
# get_cases <- sapply(split(dups$Morrone_re, dups$NAME_all), function(x) {
#   sum(unique(x) %in% c("Neotropical", "Palearctic", "Neotropical_temperate")) >= 2
# })
# dups <- dups[dups$NAME_all %in% names(get_cases)[get_cases], ]

# Calculate the area inside each realm and merge it back
dups$realmArea_km2 <- terra::expanse(dups, unit = "km")
dups1 <- terra::as.data.frame(dups)
dups1_agg <- aggregate(realmArea_km2 ~ Morrone_re + NAME_all, data = dups, sum)
total_per_name <- aggregate(realmArea_km2 ~ NAME_all, data = dups1_agg, sum)
names(total_per_name)[which(names(total_per_name) == "realmArea_km2")] <- "totalArea_km2"
dups1_agg <- dplyr::left_join(dups1_agg, total_per_name, 
                              by = "NAME_all")
dups1_agg$realmPercentage <- (dups1_agg$realmArea_km2 / dups1_agg$totalArea_km2)

##### Reshape this data to wider format #####
dups1_agg_wide <- tidyr::pivot_wider(dups1_agg,
                                     id_cols = NAME_all,
                                     names_from = Morrone_re,
                                     values_from = c(realmArea_km2,
                                                     realmPercentage),
                                     names_glue = "{Morrone_re}_{.value}")
dups1_agg_wide <- as.data.frame(dups1_agg_wide)

# Initialize new columns
dups1_agg_wide$Morrone_re_new <- NA

# Get all the percentage and area column names
perc_cols <- grep("_realmPercentage$", names(dups1_agg_wide), value = TRUE)
area_cols <- grep("_realmArea_km2$", names(dups1_agg_wide), value = TRUE)

# Process each row
for(i in 1:nrow(dups1_agg_wide)) {
  # Get percentages and areas
  percentages <- unlist(dups1_agg_wide[i, perc_cols])
  areas <- unlist(dups1_agg_wide[i, area_cols])
  # Remove NA (this is goign to become 0 later)
  valid <- !is.na(percentages) & !is.na(areas)
  percentages <- percentages[valid]
  areas <- areas[valid]
  
  # Find dominant biome
  dominant_idx <- which.max(percentages)
  dominant_percentage <- percentages[dominant_idx]
  dominant_biome <- gsub("_realmPercentage$", "", names(percentages)[dominant_idx])
  dominant_area <- areas[dominant_idx]
  
  ##### Defining cases #####
  # Case 1: majority outside study area
  if(dominant_percentage <= 0.1) {
    dups1_agg_wide$Morrone_re_new[i] <- "other"
  } 
  # Case 2: single dominant biome (>90%)
  else if(dominant_percentage >= 0.9) {
    dups1_agg_wide$Morrone_re_new[i] <- dominant_biome
  }
  # Case 3: predominant biome (75-90%)
  else if(dominant_percentage >= 0.75) {
    dups1_agg_wide$Morrone_re_new[i] <- paste(dominant_biome, "predominant")
  }
  # Case 4: transition zone (10-75%)
  else {
    # Get all biomes with >=10% coverage
    transition_realms <- gsub("_realmPercentage$", "", 
                              names(percentages)[percentages >= 0.1])
    dups1_agg_wide$Morrone_re_new[i] <- paste("Transition", 
                                              paste(sort(transition_realms), collapse = "|"))
  }
}

# Calculate the area for these cases as the sum of col the contain _Areakm2
area_cols <- grep("Area_km2", names(dups1_agg_wide), value = TRUE)
dups1_agg_wide$gaz_realmArea_km2 <- apply(dups1_agg_wide[area_cols], 1, function(x) sum(x, na.rm = TRUE))

# Merge these with the latamMap and populate the new Morrone realm column
latamMap_nt1 <- terra::as.data.frame(latamMap_nt, geom = "WKT")
latamMap_nt1 <- dplyr::left_join(latamMap_nt1, dups1_agg_wide, by = "NAME_all")
latamMap_nt1$Morrone_re_new <- ifelse(is.na(latamMap_nt1$Morrone_re_new), 
                                      latamMap_nt1$Morrone_re,
                                      latamMap_nt1$Morrone_re_new)

# Now remove what duplicated
latamMap_nt2 <- latamMap_nt1[!duplicated(cbind(latamMap_nt1$NAME_all, 
                                               latamMap_nt1$Morrone_re_new)), ]
# Prepare to merge
latamMap_nt2 <- latamMap_nt2[, -which(names(latamMap_nt2) == "geometry")]
latamMap_nt2 <- latamMap_nt2[, -which(names(latamMap_nt2) == "Morrone_re")] 

# Merge with latamMap
latamMap <- terra::as.data.frame(latamMap, geom = "WKT")
latamMap <- dplyr::left_join(latamMap, 
                             latamMap_nt2[, 5:length(latamMap_nt2)],
                             by = "NAME_all")

# Get localities not found in Morrone map
# These ones need to be edited manually, unfortunately
morrone_miss <- which(is.na(latamMap$Morrone_re_new))
# All of these are 100% Neotropical
latamMap$Morrone_re_new[morrone_miss] <- "Neotropical"

# Back to vector to get area for all remaining localities
latamMap <- terra::vect(latamMap, geom = "geometry", crs = "epsg:4326")

# Localities without area in km2
get_these <- is.na(latamMap$gaz_realmArea_km2)
latamMap$gaz_realmArea_km2[get_these] <- terra::expanse(latamMap[get_these, ], "km")
# These are also 100% neotropical localities, so percentage == 100%
latamMap$Neotropical_realmPercentage[get_these] <- 1




# Crossing EFG maps now ---------------------------------------------------

# What cases occur in more than one EFG?
dups <- latamMap_efg[duplicated(latamMap_efg$NAME_all) |
                       duplicated(latamMap_efg$NAME_all, fromLast = TRUE), ]

# Then, get duplicates that do have more than a single class of realm
# There are cases like Florianópolis where the island and the continental are
# different shapefiles, but both have the same name and occur in the same realm...
# get_cases <- sapply(split(dups$Morrone_re, dups$NAME_all), function(x) {
#   sum(unique(x) %in% c("Neotropical", "Palearctic", "Neotropical_temperate")) >= 2
# })
# dups <- dups[dups$NAME_all %in% names(get_cases)[get_cases], ]

# Calculate the area inside each realm and merge it back
dups$efgArea_km2 <- terra::expanse(dups, unit = "km")
dups1 <- terra::as.data.frame(dups)
dups1_agg <- aggregate(efgArea_km2 ~ our_efg + NAME_all, data = dups, sum)
total_per_name <- aggregate(efgArea_km2 ~ NAME_all, data = dups1_agg, sum)
names(total_per_name)[which(names(total_per_name) == "efgArea_km2")] <- "efg_totalArea_km2"
dups1_agg <- dplyr::left_join(dups1_agg, total_per_name, 
                              by = "NAME_all")
dups1_agg$efgPercentage <- (dups1_agg$efgArea_km2 / dups1_agg$efg_totalArea_km2)

##### Reshape this data to wider format #####
dups1_agg_wide <- tidyr::pivot_wider(dups1_agg,
                                     id_cols = NAME_all,
                                     names_from = our_efg,
                                     values_from = c(efgArea_km2,
                                                     efgPercentage),
                                     names_glue = "{our_efg}_{.value}")
dups1_agg_wide <- as.data.frame(dups1_agg_wide)

# Initialize new columns
dups1_agg_wide$our_efg_new <- NA

# Get all the percentage and area column names
perc_cols <- grep("_efgPercentage$", names(dups1_agg_wide), value = TRUE)
area_cols <- grep("_efgArea_km2$", names(dups1_agg_wide), value = TRUE)

# Process each row
for(i in 1:nrow(dups1_agg_wide)) {
  # Get percentages and areas
  percentages <- unlist(dups1_agg_wide[i, perc_cols])
  areas <- unlist(dups1_agg_wide[i, area_cols])
  # Remove NA (this is goign to become 0 later)
  valid <- !is.na(percentages) & !is.na(areas)
  percentages <- percentages[valid]
  areas <- areas[valid]
  
  # Find dominant biome
  dominant_idx <- which.max(percentages)
  dominant_percentage <- percentages[dominant_idx]
  dominant_biome <- gsub("_efgPercentage$", "", names(percentages)[dominant_idx])
  dominant_area <- areas[dominant_idx]
  
  ##### Defining cases #####
  # Case 1: majority outside study area
  if(dominant_percentage <= 0.1) {
    dups1_agg_wide$our_efg_new[i] <- "other"
  } 
  # Case 2: single dominant biome (>90%)
  else if(dominant_percentage >= 0.9) {
    dups1_agg_wide$our_efg_new[i] <- dominant_biome
  }
  # Case 3: predominant biome (75-90%)
  else if(dominant_percentage >= 0.75) {
    dups1_agg_wide$our_efg_new[i] <- paste(dominant_biome, "predominant")
  }
  # Case 4: transition zone (10-75%)
  else {
    # Get all biomes with >=10% coverage
    transition_realms <- gsub("_efgPercentage$", "", 
                              names(percentages)[percentages >= 0.1])
    dups1_agg_wide$our_efg_new[i] <- paste("Transition",
                                           paste(sort(transition_realms), collapse = "|"))
  }
}

# Calculate the area for these cases as the sum of col the contain _Areakm2
area_cols <- grep("Area_km2", names(dups1_agg_wide), value = TRUE)
dups1_agg_wide$gaz_efgArea_km2 <- apply(dups1_agg_wide[area_cols], 1, function(x) sum(x, na.rm = TRUE))

# Merge these with the latamMap and populate the new EFG column
latamMap_efg1 <- terra::as.data.frame(latamMap_efg, geom = "WKT")
latamMap_efg1 <- dplyr::left_join(latamMap_efg1, dups1_agg_wide, by = "NAME_all")
latamMap_efg1$our_efg_new <- ifelse(is.na(latamMap_efg1$our_efg_new),
                                    latamMap_efg1$our_efg,
                                    latamMap_efg1$our_efg_new)

# Now remove what duplicated
latamMap_efg2 <- latamMap_efg1[!duplicated(cbind(latamMap_efg1$NAME_all, 
                                               latamMap_efg1$our_efg_new)), ]
# Prepare to merge
latamMap_efg2 <- latamMap_efg2[, -which(names(latamMap_efg2) == "geometry")]
latamMap_efg2 <- latamMap_efg2[, -which(names(latamMap_efg2) == "our_efg")] 

# Merge with latamMap
latamMap <- terra::as.data.frame(latamMap, geom = "WKT")
latamMap <- dplyr::left_join(latamMap, 
                             latamMap_efg2[, c(5, 12:length(latamMap_efg2))],
                             by = "NAME_all")

# Get localities not found in Morrone map
# These ones need to be edited manually, unfortunately
morrone_miss <- which(is.na(latamMap$our_efg_new))

# Editing EFGs based on map visualization (I am leaving this explicit so if
# necessary we can easily edit each one of them separately)
# Not even Olson has this one
latamMap$our_efg_new[latamMap$NAME_all %in% "antigua barbuda_redonda_NA_NA_505"] <- "Tropical-subtropical dry forests and thickets"
# Little island absent from the map
latamMap$our_efg_new[latamMap$NAME_all %in% "argentina_entre rios_isla_NA_244"] <- "Tropical-subtropical dry forests and thickets"
# All other Bahamas are transition between intertidal forests and shrulands and savannas...
latamMap$our_efg_new[latamMap$NAME_all %in% "bahamas_grand cay_NA_NA_527"] <- "Transition Intertidal forests and shrublands|Pyric tussock savannas"
latamMap$our_efg_new[latamMap$NAME_all %in% "bahamas_harbour island_NA_NA_528"] <- "Transition Intertidal forests and shrublands|Pyric tussock savannas"
latamMap$our_efg_new[latamMap$NAME_all %in% "bahamas_spanish wells_NA_NA_545"] <- "Transition Intertidal forests and shrublands|Pyric tussock savannas"
# IUCN has no good data for Bermudas, but OneEarth says it is a mixture of 
# mangroves and subtropical forest dominated by junipers. Since these are coniferous species,
# I am going for a transition between intertidal and dry forest
latamMap$our_efg_new[latamMap$NAME_all %in% "bermuda_devonshire_NA_NA_553"] <- "Transition Intertidal forests and shrublands|Tropical-subtropical dry forests and thickets"
latamMap$our_efg_new[latamMap$NAME_all %in% "bermuda_hamilton_NA_NA_554"] <- "Transition Intertidal forests and shrublands|Tropical-subtropical dry forests and thickets"
latamMap$our_efg_new[latamMap$NAME_all %in% "bermuda_hamilton municipality_NA_NA_555"] <- "Transition Intertidal forests and shrublands|Tropical-subtropical dry forests and thickets"
latamMap$our_efg_new[latamMap$NAME_all %in% "bermuda_paget_NA_NA_556"] <- "Transition Intertidal forests and shrublands|Tropical-subtropical dry forests and thickets"
latamMap$our_efg_new[latamMap$NAME_all %in% "bermuda_pembroke_NA_NA_557"] <- "Transition Intertidal forests and shrublands|Tropical-subtropical dry forests and thickets"
latamMap$our_efg_new[latamMap$NAME_all %in% "bermuda_saint george's_NA_NA_558"] <- "Transition Intertidal forests and shrublands|Tropical-subtropical dry forests and thickets"
latamMap$our_efg_new[latamMap$NAME_all %in% "bermuda_saint george municipality_NA_NA_559"] <- "Transition Intertidal forests and shrublands|Tropical-subtropical dry forests and thickets"
latamMap$our_efg_new[latamMap$NAME_all %in% "bermuda_sandys_NA_NA_560"] <- "Transition Intertidal forests and shrublands|Tropical-subtropical dry forests and thickets"
latamMap$our_efg_new[latamMap$NAME_all %in% "bermuda_smiths_NA_NA_561"] <- "Transition Intertidal forests and shrublands|Tropical-subtropical dry forests and thickets"
latamMap$our_efg_new[latamMap$NAME_all %in% "bermuda_southampton_NA_NA_562"] <- "Transition Intertidal forests and shrublands|Tropical-subtropical dry forests and thickets"
latamMap$our_efg_new[latamMap$NAME_all %in% "bermuda_warwick_NA_NA_563"] <- "Transition Intertidal forests and shrublands|Tropical-subtropical dry forests and thickets"
# Both Espírito Santo Islands are moist broadleaf
latamMap$our_efg_new[latamMap$NAME_all %in% "brazil_espirito santo_ilha trindade_NA_2871"] <- "Tropical-subtropical lowland rainforests"
latamMap$our_efg_new[latamMap$NAME_all %in% "brazil_espirito santo_ilhas martim vaz_NA_2872"] <- "Tropical-subtropical lowland rainforests"
# San Andres y Providencia are moist broadleaf
latamMap$our_efg_new[latamMap$NAME_all %in% "colombia_san andres y providencia_n.a. (1)_NA_12242"] <- "Tropical-subtropical lowland rainforests"
# Felipe Carrillo is inside the Yucatán moist forests
latamMap$our_efg_new[latamMap$NAME_all %in% "mexico_quintana roo_felipe carrillo_NA_16148"] <- "Tropical-subtropical lowland rainforests"
# Panama is a mix of things...it depends on the exactly location
latamMap$our_efg_new[latamMap$NAME_all %in% "panama_panama_balboa_saboga_17352"] <- "Tropical-subtropical lowland rainforests"
latamMap$our_efg_new[latamMap$NAME_all %in% "panama_panama_chepo_chepillo_17355"] <- "Tropical-subtropical lowland rainforests"
latamMap$our_efg_new[latamMap$NAME_all %in% "panama_panama oeste_taboga_otoque occidente_17455"] <- "Tropical-subtropical lowland rainforests"
latamMap$our_efg_new[latamMap$NAME_all %in% "panama_panama oeste_taboga_otoque oriente_17456"] <- "Tropical-subtropical lowland rainforests"
latamMap$our_efg_new[latamMap$NAME_all %in% "panama_panama oeste_taboga_taboga_17457"] <- "Tropical-subtropical lowland rainforests"
# Peru locations are next to Titicaca, so both of them are alpine grasslands
latamMap$our_efg_new[latamMap$NAME_all %in% "peru_puno_puno_amantani_19188"] <- "Tropical alpine grasslands and shrublands"
latamMap$our_efg_new[latamMap$NAME_all %in% "peru_puno_yunguyo_anapia_19221"] <- "Tropical alpine grasslands and shrublands"
# British Virgin Islands are transition of dry and moist forests
latamMap$our_efg_new[latamMap$NAME_all %in% "british virgin islands_other islands_NA_NA_20552"] <- "Transition Tropical-subtropical dry forests and thickets|Tropical-subtropical lowland rainforests"
# Rio de Janeiro Paquetá Island is lowland
latamMap$our_efg_new[latamMap$NAME_all %in% "brazil_rio janeiro_sao goncalo_ilha paqueta_7865"] <- "Tropical-subtropical lowland rainforests"
# Last one is desertic Peru
latamMap$our_efg_new[latamMap$NAME_all %in% "peru_callao_callao_callao_18217"] <- "Hyper-arid deserts"

# Back to vector to get area for all remaining localities
latamMap <- terra::vect(latamMap, geom = "geometry", crs = "epsg:4326")

# Localities without area in km2
get_these <- is.na(latamMap$gaz_efgArea_km2)
latamMap$gaz_efgArea_km2[get_these] <- terra::expanse(latamMap[get_these, ], "km")

# rm heavy objects
rm(latamMap_nt, latamMap_nt1, latamMap_nt2,
   latamMap_efg, latamMap_efg1, latamMap_efg2,
   dups, dups1, dups1_agg, dups1_agg_wide,
   total_per_name); gc()

# Back to data.frame to simplify the process...
latamMap <- terra::as.data.frame(latamMap, geom = "WKT")

# Which cases the EFG is 'pure' and the values from gaz_efgArea_km2 needs to go
# to efg area? (and percentage)
get_these <- !grepl("predominant|\\|", latamMap$our_efg_new)

# 2. For each row:
for(i in which(get_these)) {
  efg_name <- latamMap$our_efg_new[i]
  percentage_col <- paste0(efg_name, "_efgPercentage")
  area_col <- paste0(efg_name, "_efgArea_km2")

  if(percentage_col %in% names(latamMap)) {
    latamMap[i, percentage_col] <- 1  # Set percentage to 1 (pure efg)
  }
  if(area_col %in% names(latamMap)) {
    latamMap[i, area_col] <- latamMap$gaz_efgArea_km2[i]  # Copy area value
  }
}

# Finally, convert to SpatVect and save
latamMap <- terra::vect(latamMap, geom = "geometry", crs = "epsg:4326")
file_name2 <- "latamMap_classified.gpkg"
terra::writeVector(latamMap, file.path(file_path, file_name2), overwrite = TRUE)

rm(list=ls())

