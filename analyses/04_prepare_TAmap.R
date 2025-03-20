
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
# Ecoregions 2017
er <- terra::vect(here::here("data", "raw-data", "maps", "Ecoregions2017.shp"))
# Morrone 2022
nt <- terra::vect(here::here("data", "raw-data", "maps", "morrone2022.shp"))
# Remove what is not considered Neotropic in Morrone 2022
nt <- nt[!nt$Region %in% "N/A", ]

# Subset and rename only relevant columns from each SpatVector
keep <- c("ECO_NAME", "BIOME_NAME", "REALM", "COLOR", "COLOR_BIO")
er <- er[, keep]

keep <- c("Provincias", "Region", "Subregion", "Dominio")
nt <- nt[, keep]
names(nt)[which(names(nt) == "Provincias")] <- "Morrone_provinces"
names(nt)[which(names(nt) == "Region")] <- "Morrone_region"
names(nt)[which(names(nt) == "Subregion")] <- "Morrone_subregion"
names(nt)[which(names(nt) == "Dominio")] <- "Morrone_domain"

# Now, remove what is not considered Neotropic realm by masking the ecoregions
# into this final dissolved map
nt_dis <- terra::fillHoles(terra::aggregate(nt, dissolve = TRUE))

# Crop the global Ecoregions into the Neotropics
er_final <- terra::crop(er, terra::ext(nt_dis))
# Mask the ecoregions into the Neotropics
er_final <- terra::intersect(er_final, nt_dis)
# Id, tag, and save
er_final$ID <- 1:dim(er_final)[1]
er_final$Morrone_def <- "Neotropics"

# Load SynTreeSys map to check which ecoregions are missing and we need to
# manually classify

##### I am hoping Géraldine has this information here #####


# Save everything
file_path <- here::here("data", "derived-data", "maps")

if(!dir.exists(file_path))
  dir.create(file_path)

# Morrone 2022 Neotropical map, complete (without N/A)
file_name1 <- "Neotropic_complete.shp"
terra::writeVector(nt, file.path(file_path, file_name1), overwrite = TRUE)

# Morrone 2022 Neotropical map, dissolved (without N/A)
file_name2 <- "Neotropic_dissolved.shp"
terra::writeVector(nt_dis, file.path(file_path, file_name2), overwrite = TRUE)

# Ecoregions 2017 masked into the Neotropical map
file_name3 <- "Neotropical_ecoregions.shp"
terra::writeVector(er_final, file.path(file_path, file_name3), overwrite = TRUE)

# And the data.frame to edit
writexl::write_xlsx(terra::as.data.frame(er_final), file.path(file_path, "Neotropical_ecoregions.xlsx"))

