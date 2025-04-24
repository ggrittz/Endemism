
# Preparing tropical American map -----------------------------------------

# Downloading ecorregions Dinerstein et al. (2017)
file_path <- here::here("data", "raw-data")
file_name <- "ecoregions2017.zip"
url <- "https://storage.googleapis.com/teow2016/Ecoregions2017.zip"
download.file(url, file.path(file_path, file_name))

file_path1 <- here::here("data", "raw-data", "maps")
if(!dir.exists(file_path1))
  dir.create(file_path1)

# Extracting, reading, and preparing
unzip(zipfile = file.path(file_path, file_name),
      exdir = file_path1)

# Zenodo's shapefile for the Tropical America
# https://zenodo.org/api/records/14278747/files-archive
# Manually downloaded since Zenodo is closed-access

# Morrone 2022 map was also manually downloaded from
# https://neotropicalmap.atlasbiogeografico.com/ available Google Driver folder

# Preparing ecoregions based on Morrone 2022

# Loading maps

# terra::is.valid() both maps contain invalid geometries...nothing that can be done about this

# Ecoregions 2017
er <- terra::vect(here::here("data", "raw-data", "maps", "Ecoregions2017.shp"))
# Morrone 2022
nt <- terra::vect(here::here("data", "raw-data", "maps", "morrone2022.shp"))

# Subset and rename only relevant columns from each SpatVector
keep <- c("ECO_NAME", "BIOME_NAME")
er <- er[, keep]

keep <- c("Region", "Subregion", "Dominio")
nt <- nt[, keep]
names(nt)[which(names(nt) == "Region")] <- "Morrone_region"
names(nt)[which(names(nt) == "Subregion")] <- "Morrone_subregion"
names(nt)[which(names(nt) == "Dominio")] <- "Morrone_domain"

# Keep only Neotropics here
nt_dis <- terra::fillHoles(terra::aggregate(nt[!nt$Morrone_region %in% "N/A", ],
                                            dissolve = TRUE, cores = 8))
nt_dis$Morrone_region <- "Neotropical"

# Keep only N/A here
other_dis <- terra::fillHoles(terra::aggregate(nt[nt$Morrone_region %in% "N/A", ],
                                               dissolve = TRUE, cores = 8))
other_dis$Morrone_region <- NA

# Disaggregate to separate into Nearctic and "temperate Neotropics" based on latitude
other_dis <- terra::disagg(other_dis)
centroids <- terra::centroids(other_dis)
coords <- terra::crds(centroids)

# Classify based on latitude (y-coordinate)
other_dis$Morrone_region <- ifelse(coords[, "y"] >= 0, "Palearctic", "Neotropical_temperate")
# Aggregate back based on region
other_dis <- terra::aggregate(other_dis, by = "Morrone_region", dissolve = TRUE, cores = 8)
other_dis <- other_dis[, -which(names(other_dis) == "agg_n")]

# Now separate finally into Palearctic and below Neotropics
paleartic <- other_dis[other_dis$Morrone_region %in% "Palearctic", ]
below_nt <- other_dis[other_dis$Morrone_region %in% "Neotropical_temperate", ]

# Test
# terra::plot(nt_dis, col = 'green')
# terra::plot(paleartic, col = 'red', add = T)
# terra::plot(below_nt, col = 'blue', add = T)

# Bind it into a single shapefile
nt_complete <- rbind(nt_dis, paleartic, below_nt)

# Crop the global Ecoregions into the Neotropical realm
er_final1 <- terra::crop(er, terra::ext(nt_complete))
# Mask the ecoregions into the Neotropics
er_final1 <- terra::intersect(er_final1, nt_complete)
# Do the same for Nearctic and Antarctic
er_final2 <- terra::crop(er, terra::ext(other_dis))
er_final2 <- terra::intersect(er_final2, other_dis)

# Join these for visualization
er_final3 <- rbind(er_final1, er_final2)

# Loading our new classification (sts previously done + ours)
sts_class <- readxl::read_xlsx(here::here("data", "derived-data", "maps",
                                          "final_classification.xlsx"))

# Remove latin before merging
er_final3$to_merge <- plantR::rmLatin(er_final3$ECO_NAME)
sts_class$to_merge <- plantR::rmLatin(sts_class$ECO_NAME)
efg_map <- terra::as.data.frame(er_final3, geom = "WKT")
efg_map <- dplyr::left_join(efg_map, 
                           sts_class[, c("to_merge", "our_efg")],
                           by = "to_merge")

# Any duplicated lines?
efg_map <- efg_map[!duplicated(efg_map), ]
# Remove two ecoregions that are not in the Americas
efg_map <- efg_map[!efg_map$ECO_NAME %in% c("Rapa Nui and Sala y Gómez subtropical forests",
                                           "Scotia Sea Islands tundra"), ]
 
# Back to Vect
efg_map <- terra::vect(efg_map, geom = "geometry", crs = "epsg:4326")
efg_map <- efg_map[, -which(names(efg_map) == "to_merge")]

# Save everything
file_path <- here::here("data", "derived-data", "maps")

if(!dir.exists(file_path))
  dir.create(file_path)

# Morrone 2022 Neotropical map and pale+temperate
file_name1 <- "NTAmericas_dissolved.shp"
terra::writeVector(nt_complete, file.path(file_path, file_name1), overwrite = TRUE)

# Ecoregions 2017 masked into the Neotropical map
file_name3 <- "NTAmericas_ecoregions.shp"
terra::writeVector(efg_map, file.path(file_path, file_name3), overwrite = TRUE)

# No longer needed since all information is in final_classification.xlsx
# And the data.frame to edit
# writexl::write_xlsx(terra::as.data.frame(efg_map), file.path(file_path, "Neotropical_ecoregions.xlsx"))


##### Preparing EFGs map #####

# Add a buffer of size 0 to deal with some (probable) geometry issues
efg_map1 <- terra::buffer(efg_map, 0)
efg_map1 <- terra::aggregate(efg_map1, by = "our_efg", dissolve = TRUE, cores = 8)

# Testing if everything is ok
cores <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD",
           "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22", "#17BECF",
           "#AEC7E8", "#FFBB78", "#98DF8A", "#FF9896", "#C5B0D5",
           "#C49C94", "#F7B6D2", "#C7C7C7", "#DBDB8D", "#9EDAE5",
           "#393B79", "#637939", "#8C6D31", "#843C39", "#7B4173")

# Testing
terra::plot(efg_map1, col = cores[as.factor(efg_map1$our_efg)]) # all good

# Get total areas
efg_map1$area_ha <- terra::expanse(efg_map1, unit = "ha")
efg_map1$area_km2 <- efg_map1$area_ha * 0.01


# Save the efg map
file_name <- "NTAmericas_efgs.shp"
terra::writeVector(efg_map1, file.path(file_path, file_name), overwrite = TRUE)

