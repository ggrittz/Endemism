# Prepare WDPA ------------------------------------------------------------
# Terrestrial protected areas of tropical America (IUCN categories Ia, Ib, II
# and III; designated or established), from WDPA February 2026
# Output: data/derived-data/maps/tropicalAmerica_WDPA_Feb2026_cleaned.shp (read by 08)

# Tropical America (as in 08_preparing_plots.R)
tropical_america_diss <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp"))
tropical_america_diss <- tropical_america_diss[tropical_america_diss$Morrone_re %in% "Neotropical", ]
tropical_america_diss <- terra::fillHoles(terra::aggregate(tropical_america_diss, cores = 8))

# Download and unzip (nested zip files)
options(timeout = 10000)
zip_name <- here::here("data", "raw-data", "WDPA_Feb_2026.zip")
if (!file.exists(zip_name))
  download.file(url = "https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Feb2026_Public_shp.zip",
                destfile = zip_name)
unzip(zipfile = zip_name, exdir = here::here("data", "raw-data", "WDPA_Feb_2026"))
zip_files <- list.files(path = here::here("data", "raw-data", "WDPA_Feb_2026"),
                        pattern = ".zip", full.names = TRUE)
for (i in seq_along(zip_files)) {
  output_path <- here::here("data", "raw-data", paste0("WDPA_Feb_2026_shapefiles_", i - 1))
  if (!dir.exists(output_path))
    dir.create(output_path, recursive = TRUE)
  unzip(zip_files[i], exdir = output_path)
}

# Filter, crop to tropical America and save
zip_folders <- list.files(here::here("data", "raw-data"), pattern = "_shapefiles_", full.names = TRUE)
all_polygons <- lapply(zip_folders, function(x) {
  wdpa <- sf::st_read(file.path(x, "WDPA_Feb2026_Public_shp-polygons.shp"))
  wdpa <- wdpa[wdpa$IUCN_CAT %in% c("Ia", "Ib", "II", "III") &
                 wdpa$REALM %in% "Terrestrial" &
                 wdpa$STATUS %in% c("Designated", "Established"), ]
  wdpa <- terra::crop(terra::vect(wdpa), terra::ext(tropical_america_diss))
  wdpa <- terra::buffer(wdpa, 0)
  terra::intersect(wdpa, tropical_america_diss)
})
terra::writeVector(terra::vect(all_polygons),
                   here::here("data", "derived-data", "maps", "tropicalAmerica_WDPA_Feb2026_cleaned.shp"),
                   overwrite = TRUE)
rm(tropical_america_diss, zip_name, zip_files, output_path, zip_folders, all_polygons, i)
