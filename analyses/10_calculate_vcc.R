
# Relate climate change velocity and endemism -----------------------------

# Loading climate data and interpolated weighted endemism
chelsa_path <- list.files(here::here("data", "derived-data", "chelsa"),
                          full.names = TRUE)
chelsa <- terra::rast(chelsa_path[grepl("100km", chelsa_path)])

# Make RasterStack
chelsa_rs <- raster::stack(chelsa_path[grepl("100km", chelsa_path)])

# Load WE
we_path <- here::here("data", "derived-data", "maps", "we_area_kriging_100km.tiff")
we_map <- terra::rast(we_path)

##### Climate change velocity metrics #####

# Hamnan et al. 2015
dv <- climetrics::dVelocity(chelsa, t1 = c(1:2), t2 = c(2:3), ny = 100)
plot(dv,col=hcl.colors(100), 
     main='Distance-based Velocity of Climate Change for Precipitation')

?VoCC::gVoCC()
?VoCC::tempTrend

# Temporal Trend
tt <- VoCC::tempTrend(chelsa_rs, th = 1)
# Spatial Gradiant
sg <- VoCC::spatGrad(chelsa_rs, th = 1, projected = TRUE)

# Gradient-Based CCV
cvv <- VoCC::gVoCC(tt, sg)
cvv <- terra::rast(cvv)

we_map_df <- terra::as.data.frame(we_map, xy = TRUE)
cvv_df <- terra::as.data.frame(cvv, xy = TRUE)

plot(we_map_df$we.pred, cvv_df$voccMag, pch = 19, cex = 0.5)

gam_mod <- mgcv::gam(we_map_df$we.pred ~ s(cvv_df$voccMag),
                     family = gaussian(link = "log"),
                     method = "REML")
summary(gam_mod)
plot(gam_mod)
mgcv::gam.check(gam_mod)





