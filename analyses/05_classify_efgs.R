





##### REMOVE THIS SCRIPT AFTER GETTING THE COOL COLOR PALETTE #####








# This script seems useless now since everything is been done in 04_
# But confirm later
# Classify missing EFGs ---------------------------------------------------

# Reading the map
file_path <- here::here("data", "derived-data", "maps")
file_name <- "NTAmericas_ecoregions.shp"
nt_efg <- terra::vect(file.path(file_path, file_name))
terra::crs(nt_efg) <- "epsg:4326"

# Reading the old classified ecoregions
old_class <- readxl::read_xlsx(file.path(file_path, "Neotropical_ecoregions.xlsx"))
# Reading the newly classified ecoregions
new_class <- readxl::read_xlsx(file.path(file_path, "final_classification.xlsx"))
attrib_table <- terra::as.data.frame(nt_efg, geom = "WKT")

# Merge with new classification
merged_data <- dplyr::left_join(attrib_table, 
                                new_class[, c("ECO_NAME", "our_efg", 
                                              "our_reviewer", "our_comments")], 
                                by = "ECO_NAME")
# Remove dup
merged_data <- merged_data[!duplicated(merged_data), ]

# Save this to re-upload to Drive
# file_name1 <- "Neotropical_ecoregions_edited_new.xlsx"
# writexl::write_xlsx(merged_data[, -which(names(merged_data) == "geometry")], file.path(file_path, file_name1))

# Making palette colors
efgs <- sort(unique(merged_data$our_efg[merged_data$Morrone_re %in% "Neotropical"]))
cores <- c("#ADD8E6", "#F5F5DC", "#6e7c56",
           "#e6e5d7", "#c1b6d2", "#eb8e8a", 
           "#c5949c", "#90b0c4", "#e3b376", 
           "#d7c764", "#abc077", "#fffff2", 
           "#f0cfd4", "#e8a0de", "#90EE90", 
           "#d1c1d2", "#bc85bd", "#c4c4c4",
           "#cfdf99", "#feec6e")


# col_palette <- c(
#   "#3d6a4a",  # Boreal high montane (cool dark green)  
#   "#5d8b61",  # Boreal montane (soft evergreen)  
#   "#d9c8a3",  # Cool desert (pale sand)  
#   "#6b8e23",  # Deciduous temperate (classic olive)  
#   "#7a9d6d",  # Heath forests (muted moss)  
#   "#e5d7a3",  # Hyper arid desert (warm sand)  
#   "#6a8b7d",  # Intertidal (coastal green-gray)  
#   "#2e5e3a",  # Montane rainforest (deep green)  
#   "#3a7150",  # Oceanic cool temperate (rainforest teal)  
#   "#3a7150",  # (duplicate, same as above)  
#   "#7faac8",  # Permanent marshes (water blue)  
#   "#c9b45e",  # Pyric savanna (golden grass)  
#   "#8fb3aa",  # Floodplain marshes (sage green)  
#   "#b8aa72",  # Dry temperate heath (tan)  
#   "#b8aa72",  # (duplicate, same as above)  
#   "#d2b48c",  # Tropical dry shrubland (light khaki)  
#   "#c4b56e",  # Semi-desert steppe (dusty yellow)  
#   "#5d8b7d",  # Forested wetlands (teal-green)  
#   "#e5d1b8",  # Succulent desert (warm beige)  
#   "#8a9d6e",  # Alpine grassland (muted green)  
#   "#a3b86c",  # Subhumid grassland (lime-tinged)  
#   "#8a9a5b",  # Temperate woodland (sage)  
#   "#9caf88",  # Tropical alpine (pale green)  
#   "#c9a66b",  # Tropical dry forest (golden-brown)  
#   "#6a9a6f",  # Tropical flooded forest (emerald)  
#   "#4a7d4a"   # Tropical lowland rainforest (rich green)  
# )

col_palette <- cores[1:length(efgs)]
names(col_palette) <- efgs

# Convert back to SpatVector
final_map <- terra::vect(merged_data, geom = "geometry")
terra::crs(final_map) <- "epsg:4326"

# Dissolve using sf since the final_map will have invalid geometries due to 
# input maps
final_map1 <- terra::buffer(final_map, 0)
final_map1 <- terra::aggregate(final_map1, by = "our_efg", dissolve = TRUE, cores = 8)
# Get total areas
final_map1$area_ha <- terra::expanse(final_map1, unit = "ha")
final_map1$area_km2 <- final_map1$area_ha * 0.01


# No longer needed since we are now classifying in script 04_prepareTAmap.R
# Save these newly edited  maps
# file_name1 <- "Neotropical_efgs_reclassified.shp"
# terra::writeVector(final_map1, file.path(file_path, file_name1), overwrite = TRUE)
# file_name2 <- "Neotropical_ecoregions_reclassified.shp"
# terra::writeVector(final_map, file.path(file_path, file_name2), overwrite = TRUE)
# file_name3 <- "final_classification.xlsx"
# writexl::write_xlsx(merged_data[, -which(names(merged_data) == "geometry")], 
#                     file.path(file_path, file_name3))


##### Plotting test #####
# Test
# terra::plot(nt_efg)
# 
# cores <- rainbow(length(unique(final_map1$our_efg)))
# terra::plot(final_map1, 
#             col = cores[as.factor(final_map1$our_efg)],
#             main = "Neotropical Ecossystem functional groups",
#             add = TRUE)
# # terra::north(xy = "topright", type = 2)
# terra::sbar(d = 1000, xy = "bottomright", 
#             type = "bar", div = 4, 
#             lonlat = TRUE, below = "km")
# 
# legend(x = -117.5,
#        y = 15.25,
#        title = "EFG",
#        legend = names(col_palette),
#        fill = col_palette,
#        y.intersp = 0.6,
#        x.intersp = 0.45,
#        cex = 0.6,
#        ncol = 1,
#        bty = "n")





  