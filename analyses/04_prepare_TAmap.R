

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

# Crop the ecoregions into the Neotropical realm extent
nt_ext <- terra::ext(nt)
er_cropped <- terra::crop(er, nt_ext)
terra::plot(er_cropped)

morrone_df <- terra::as.data.frame(morrone)

morrone_ext <- terra::ext(morrone)
terra::plot(morrone_ext, 'red')

er <- here::here("data", "raw-data", "maps", "Ecoregions2017.shp")
er <- terra::vect(er)
er_neotrop <- terra::crop(er, morrone_ext)
er_neotrop_df <- terra::as.data.frame(er_neotrop)

# Géraldine
gera <- here::here("data", "raw-data", "maps", "Biom_agg_forested.shp")
gera <- terra::vect(gera)
gera <- terra::as.data.frame(gera)

#







