
# Calculate endemism ------------------------------------------------------
devtools::load_all()

# gaz <- read.csv(here::here("data", "raw-data", "raw-gazetteer.csv"))
# 
# regions <- rnaturalearth::ne_countries(returnclass = "sv")
# regions <- regions[regions$subregion %in% c("Caribbean", "Central America",
#                                             "South America", "Northern America"), ]
# regions <- regions[, c("iso_a2", "subregion")]
# regions <- terra::as.data.frame(regions, geom = NULL)
# 
# gaz <- dplyr::left_join(gaz, regions, by = c("country_code" = "iso_a2"))
# gaz <- gaz[!is.na(gaz$subregion), ]
# a <- gaz[!duplicated(gaz$NAME_0) & gaz$resolution.gazetteer %in% c("state", "country"), ]
# 
# 
# # Check which gazetteer's localities do not fall in the map...
# 
# names(gaz)[match(c("longitude.gazetteer", "latitude.gazetteer", 
#                    "NAME_0", "NAME_1", "NAME_2", "NAME_3", "resolution.gazetteer"), 
#                  names(gaz))] <- 
#   c("decimalLongitude.new", "decimalLatitude.new", "country.new", 
#     "stateProvince.new", "municipality.new", "locality.new", "resol.orig")
# gaz$locality.scrap <- NA
# 
# # Get and simplify locality string create by strLoc
# locs <- plantR::strLoc(gaz)
# # Simplify
# locs$loc.string <- plantR::prepLoc(locs$loc.string) # priority string
# locs$loc.string1 <- plantR::prepLoc(locs$loc.string1) # alternative string 1
# locs$loc.string2 <- plantR::prepLoc(locs$loc.string2) # alternative string 2
# 
# # Cross the simplified strings against plantR's gazetteer
# locs <- plantR::getLoc(locs, gazet = "plantR")
# 
# gaz <- cbind.data.frame(gaz, locs[, c("loc",
#                                       "loc.correct",
#                                       "latitude.gazetteer",
#                                       "longitude.gazetteer",
#                                       "resolution.gazetteer")])
# gaz$decimalLongitude <- gaz$decimalLongitude.new
# gaz$decimalLatitude <- gaz$decimalLatitude.new
# 
# # Validating locality information
# # gaz <- plantR::validateLoc(gaz)
# gaz <- plantR::getCoord(gaz)
# # Validating original coordinates at country, state, and county level
# gaz <- plantR::checkCoord(gaz,
#                           # orig.coord = NULL,
#                           keep.cols = c("geo.check", 
#                                         "NAME_0",
#                                         "country.gazet"))
# 
# # Validating coordinates due to rounding precision
# gaz <- plantR::checkBorders(gaz, output = "same.col")
# 
# # Validating coordinates in the sea but close to the shore
# gaz <- plantR::checkShore(gaz, output = "same.col")



# Check above later: cross gazetteer with prepLoc/validateLoc

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

  all_fam[[i]] <- readRDS(family_path)[, c("decimalLongitude.new", "decimalLatitude.new",
                                       "scientificName", "family", "our_efg", "is_nt", "efg_endemic",
                                       "loc.correct", "tax.check", "identifiedBy.new")]
}

# Bind it all
all_fam <- dplyr::bind_rows(all_fam)
# Keep only records validated by a taxonomists ($tax.check == 'high')
# high_names <- unique(all_fam$scientificName[all_fam$tax.check %in% "high"])
all_fam1 <- all_fam[all_fam$tax.check %in% "high", ]
# Get only "trees" lifeForm
all_fam1 <- dplyr::left_join(all_fam1, our_names1[, c("our_accepted_scientificName", 
                                                     "lifeForm_aggregated")],
                             by = c("scientificName" = "our_accepted_scientificName"))
all_fam1 <- all_fam1[grepl("tree", all_fam1$lifeForm_aggregated), ]

# Dealing with transition zones and predominance --------------------------
# How many registers have 'Transition' or 'predominant' in its name?
table(grepl("\\|", all_fam1$our_efg) |
        grepl("predominant", all_fam1$our_efg)) # 23721 / 999552 (2%)

# For richness, split 'Transition EFGs' to count the register for all efgs it transitions
expanded_rows <- as.data.frame(tidyr::separate_rows(all_fam1, our_efg, sep = "\\|"))
# tidyr is extremely much faster
# expanded_rows <- lapply(seq_len(nrow(all_fam1)), function(i) {
#   efg_split <- unlist(strsplit(all_fam1$our_efg[i], "\\|"))
#   data.frame(scientificName = rep(all_fam1$scientificName[i], length(efg_split)),
#              our_efg = efg_split)
# })
# expanded_rows <- dplyr::bind_rows(expanded_rows)
expanded_rows$our_efg <- gsub("Transition ", "", expanded_rows$our_efg)
# Keep only unique cases
expanded_df <- expanded_rows[!duplicated(expanded_rows), ]
# Merge back
# expanded_df <- plyr::rbind.fill(all_fam1[!composite_efg, ], expanded_rows)
# For richness, we are considering predominant to be the indicated EFG
expanded_df$our_efg <- gsub(" predominant", "", expanded_df$our_efg)
# expanded_df$efg_endemic <- gsub(" predominant", "", expanded_df$efg_endemic)

# Now, keep only Neotropical endemics here
expanded_df <- expanded_df[expanded_df$is_nt %in% TRUE, ]

##### Richness per EFG #####
richness <- aggregate(scientificName ~ our_efg, 
                      data = expanded_df, 
                      FUN = function(x) length(unique(x)))

##### Endemics per EFG #####
# For endemics, do not count transitions nor predominance as is; keep separated
endemic_species <- unique(expanded_df[!is.na(expanded_df$efg_endemic), 
                                      c("efg_endemic", "scientificName")])
endemic_count <- as.data.frame(table(endemic_species$efg_endemic))
names(endemic_count) <- c("our_efg", "endemics")

# Merging
result <- dplyr::left_join(richness, endemic_count, by = "our_efg")
names(result) <- c("EFG", "Richness", "Endemics")
result$Endemism_level <- round(100 * (result$Endemics / result$Richness), 2) 

# Generate species lists per EFG
species_list <- lapply(split(expanded_df, expanded_df$our_efg), 
                       function(x) unique(x$scientificName))

# Generate species lists per EFG (endemics)
species_list_end <- lapply(split(expanded_df, expanded_df$efg_endemic), 
                           function(x) unique(x$scientificName))

# Save these
# path_to_save <- here::here("data", "derived-data", "rds")
# file1 <- "spp_per_efg.rds"
# file2 <- "end_per_efg.rds"
# saveRDS(species_list, file.path(path_to_save, file1))
# saveRDS(species_list_end, file.path(path_to_save, file2))


# What names are missing from our registers? (only good taxonomic sources here)
all_trees <- our_names1$our_accepted_scientificName[
  grepl("tree",our_names1$lifeForm_aggregated) & 
    grepl("FBO|GTS|VPA|WCVP", our_names1$source)]

# These registers occur in the Neotropics but we didn't found a good taxonomist
# to vouch for them
missing_trees <- setdiff(all_trees, unique(all_fam1$scientificName))



# Weighted Endemism -------------------------------------------------------

# Grid cell resolution to be used
base_res <- 111.32/ 111.32 # 111.32 km = 1 grid degree at Equator

# Create template raster
r_template <- terra::rast(extent = terra::ext(tropical_america),
                          resolution = base_res,
                          crs = "epsg:4326")

# Rasterize the Tropical America map into the template
raster_output <- terra::rasterize(tropical_america, r_template, field = "Morrone_re")
# Save this product for later
# terra::writeRaster(raster_output, 
#                    file.path(here::here("data",
#                                         "derived-data",
#                                         "maps",
#                                         "nt_100km.tiff")),
#                    overwrite = TRUE)

# Convert to {raster} since the Guerin 2015 function was made on it
# Check for our own updates if time makes it possible #
raster_raster <- raster::raster(raster_output)

# Simple test
# all_fam2 <- all_fam1[1:500000, ]
all_fam2 <- all_fam1[all_fam1$is_nt %in% TRUE, ]

# Mapping weighted endemism using 'span'
# weighting method, either the maximum geographical distance 
# spanning the occurrences of a species (‘span’) Guerin 2015
# we_map_span = weighted.endemism(species_records = all_fam2,
#                                 species = "scientificName",
#                                 latitude = "decimalLatitude.new",
#                                 longitude = "decimalLongitude.new",
#                                 frame.raster = raster_raster,
#                                 outlier_pct = 99,
#                                 plot.raster = FALSE)


# Mapping weighted endemism using 'area' (EOO convex hull per se)
we_map_area = weighted.endemism(species_records = all_fam2,
                                species = "scientificName",
                                latitude = "decimalLatitude.new",
                                longitude = "decimalLongitude.new",
                                frame.raster = raster_raster,
                                outlier_pct = 99,
                                weight.type = "geo",
                                geo.type = "cell",
                                geo.calc = "polygon",
                                plot.raster = FALSE)
# we_map_area_null <- endemism.null.test(we_map_area,
#                                nrep = 1000,
#                                outlier.range = 1.5)

# Store both outputs for later use, if needed
# path_to_save <- here::here("data", "derived-data", "rds")
# name_to_save1 <- "we_map_span_100km.rds"
# name_to_save2 <- "we_map_area_100km.rds"
# if(!dir.exists(path_to_save))
#   dir.create(path_to_save)
# 
# saveRDS(we_map_span, file.path(path_to_save, name_to_save1))
# saveRDS(we_map_area, file.path(path_to_save, name_to_save2))
# we_map_span <- readRDS(here::here("data", "derived-data", "rds",
#                                   "we_map_span.rds"))
we_map_area <- readRDS(here::here("data", "derived-data", "rds",
                                  "we_map_area_100km.rds"))

# # Are both metrics related?
# cor(we_map_area$WE, we_map_span$WE) # 0.96 cor
# mod <- lm(we_map_span$WE ~ we_map_area$WE)
# plot(we_map_span$WE, we_map_area$WE, pch = 19, cex = 0.75,
#      xlab = "Weighted Endemism (area)", ylab = "Weighted Endemism (span)")



# Interpolating weighted endemism -----------------------------------------
coords <- data.frame(raster::coordinates(we_map_area$WE_raster))
values <- data.frame(raster::values(we_map_area$WE_raster))
names(values) <- "we"
my_we_df <- cbind.data.frame(coords, values)
my_we_pts <- my_we_df[!is.na(my_we_df$we), ]

# Convert the {raster} map to {terra}
my_we_raster <- terra::rast(we_map_area$WE_raster)

# Grid to interpolate
my_we_grid <- terra::rast(extent = terra::ext(my_we_raster),
                          resolution = terra::res(my_we_raster),
                          crs = terra::crs(my_we_raster))
my_we_grid <- terra::as.data.frame(my_we_grid, xy = TRUE, na.rm = FALSE)
my_we_grid$lyr.1 <- NULL


##### Method 1: Ordinary Kriging interpolation #####
my_we_pts_sp <- my_we_pts
sp::coordinates(my_we_pts_sp) <- ~x+y
# Make variogram
v <- automap::autofitVariogram(we ~ 1, my_we_pts_sp)
vario <- v$var_model
krig_mod <- gstat::gstat(id = "we",
                         formula = we ~ 1,
                         locations = ~x+y,
                         data = my_we_df[!is.na(my_we_df$we), ],
                         model = gstat::vgm(model = "Ste", # find a better way to write this down
                                            psill = 1.076630e-14,
                                            range = 238594,
                                            kappa = 0.4),
                         nmax = 20)

# Predict the omdel in the grid
my_we_grid$krig.fit <- predict(krig_mod, my_we_grid)$we.pred


# Method 2: Loess interpolation -------------------------------------------
loess_mod <- loess(formula = we ~ x * y,
                   span = 0.2,
                   degree = 2,
                   surface = 'interpolate',
                   family = 'gaussian',
                   se = TRUE,
                   normalize = TRUE,
                   data = my_we_pts)

# Predict the model in the grid
my_we_grid$loess.fit <- predict(loess_mod, my_we_grid)
my_we_grid$loess.fit[is.na(my_we_grid$loess.fit)] <- 0
my_we_grid$loess.fit[my_we_grid$loess.fit < 0] <- 0


# Inverse distance weighting ----------------------------------------------
idw_mod <- gstat::gstat(id = "we",
                        formula = we ~ 1,
                        locations = ~x+y,
                        set = list(idp = 2),
                        data = my_we_pts)

# Predict the model in the grid
my_we_grid$idw.fit <- predict(idw_mod, my_we_grid)$we.pred


# Convert to raster for cropping, masking, and plotting
my_we_grid <- terra::rast(my_we_grid, type = "xyz", crs = "epsg:4326")

# Crop and mask
my_int <- terra::crop(my_we_grid, tropical_america)
my_int <- terra::mask(my_int, tropical_america)

# Get 90th quantile for each case
th_we <- as.numeric(raster::quantile(we_map_area$WE_raster, probs = 0.9, na.rm = TRUE))
th_krig <- terra::global(my_int$krig.fit, fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
th_loess <- terra::global(my_int$loess.fit, fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
th_idw <- terra::global(my_int$idw.fit, fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]

# Convert WE map to terra
we_map <- terra::rast(we_map_area$WE_raster)
terra::values(we_map) <- terra::values(we_map_area$WE_raster)
terra::plot(we_map, col = terrain.colors(100, rev = TRUE))

# Plot
par(mfrow = c(2, 2))
# WE map
terra::plot(we_map, col = terrain.colors(100, rev = TRUE), legend = FALSE,
            main = "Weigted Endemism (EOO)")
terra::contour(we_map, add = TRUE, levels = th_we, col = "black", lwd = 1)
# Kriging map
terra::plot(my_int$krig.fit, col = terrain.colors(100, rev = TRUE), legend = FALSE,
            main = "Kriging predictions")
terra::contour(my_int$krig.fit, add = TRUE, levels = th_krig, col = "black", lwd = 1)
# IDW map
terra::plot(my_int$idw.fit, col = terrain.colors(100, rev = TRUE), legend = FALSE,
            main = "IDW predictions")
terra::contour(my_int$idw.fit, add = TRUE, levels = th_loess, col = "black", lwd = 1)
# Loess map
terra::plot(my_int$loess.fit, col = terrain.colors(100, rev = TRUE), legend = FALSE,
            main = "Loess predictions")
terra::contour(my_int$loess.fit, add = TRUE, levels = th_loess, col = "black", lwd = 1)



