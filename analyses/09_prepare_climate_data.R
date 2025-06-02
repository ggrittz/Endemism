
# Prepare CHELSA climate data ---------------------------------------------

# Loading
file_path <- here::here("data", "raw-data", "chelsa")
chelsa <- terra::rast(list.files(file_path, full.names = TRUE))

# WE kriged grid at 100 km reso
we_krig <- terra::rast(here::here("data", "derived-data",
                                           "maps", "we_area_kriging_100km.tiff"))

# Same crs
# terra::crs(chelsa) == terra::crs(we_krig)

# Crop CHELSA into grid
chelsa1 <- terra::crop(chelsa, we_krig)
# Resample CHELSA to the grid resolution 
chelsa_res <- terra::resample(chelsa1, we_krig)
# Now mask since they have the same extent now
chelsa_res <- terra::mask(chelsa_res, we_krig)

# Save this product for later too
save_path <- here::here("data", "derived-data", "chelsa")
if(!dir.exists(save_path))
 dir.create(save_path)
output_files <- file.path(save_path, paste0(names(chelsa_res), "_resampled100km.tif"))
terra::writeRaster(chelsa_res, filename = output_files, overwrite = TRUE)










