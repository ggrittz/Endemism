# Rarefaction and interpolation -------------------------------------------

devtools::load_all()

# Load data
final_df <- readRDS(here::here("data", "derived-data", "rds", "final_df.rds"))
final_df$decimalLatitude.new1 <- as.numeric(final_df$decimalLatitude.new1)
final_df$decimalLongitude.new1 <- as.numeric(final_df$decimalLongitude.new1)

we_map_area <- readRDS(here::here("data", "derived-data", "rds", "we_map_area_50km.rds"))
we_map_area_biomes <- readRDS(here::here("data", "derived-data", "rds", "we_map_area_biomes_50km.rds"))

efg_end <- readRDS(here::here("data", "derived-data", "rds", "end_per_efg.rds"))
efg_end <- efg_end[names(efg_end) %in% names(we_map_area_biomes)]
efg_names <- names(efg_end)

# Study area
tropical_america <- here::here("data", "derived-data", "maps", "NTAmericas_dissolved.shp")
tropical_america <- terra::vect(tropical_america, crs = "epsg:4326")
tropical_america <- tropical_america[tropical_america$Morrone_re %in% "Neotropical", ]

# Dealing with sampling coverage ------------------------------------------

# Convert the {raster} map to {terra}
my_we_raster <- terra::rast(we_map_area$WE_raster, crs = "epsg:4326")
terra::values(my_we_raster) <- raster::values(we_map_area$WE_raster)

# Do the same for each biome map
my_we_raster_biomes <- list()
my_we_raster_biomes <- lapply(we_map_area_biomes, 
                              function(x) {
                                r <- terra::rast(x$WE_raster, crs = "epsg:4326")
                                terra::values(r) <- terra::values(x$WE_raster)
                                return(r)
                              })

# Convert occ points to spatial points
final_df1 <- terra::vect(final_df[final_df$is_realm_predominant %in% FALSE 
                                  & final_df$is_realm_transitional %in% FALSE, ], 
                         geom = c("decimalLongitude.new1", "decimalLatitude.new1"), 
                         crs = "epsg:4326")

# Get the number of endemic species to be used for biome-interpolation here
aggregate(scientificName ~ efg_endemic, data = final_df, 
          FUN = function(x) length(unique(x)))

# Get cell numbers for all points
cell_numbers <- terra::cellFromXY(my_we_raster, terra::crds(final_df1))

cell_numbers_biomes <- list()
# Do the same for biomes; but using only the endemics to it
cell_numbers_biomes <- lapply(names(my_we_raster_biomes),
                              function(x) {
                                # Get the raster for this biome
                                biome_raster <- my_we_raster_biomes[[x]]
                                # Get the endemic species for this biome
                                endemic_species <- efg_end[[x]]
                                # Subset the spatial points data for these endemic species
                                final_df2 <- final_df1[final_df1$scientificName %in% endemic_species, ]
                                # Convert raster to polygon for intersection
                                biome_polygon <- terra::as.polygons(biome_raster)
                                # Intersect points with biome polygon
                                final_df2 <- terra::intersect(final_df2, biome_polygon)
                                # Get cell numbers
                                cells <- terra::cellFromXY(biome_raster, terra::crds(final_df2))
                                cells[!is.na(cells)]
                              })
names(cell_numbers_biomes) <- names(my_we_raster_biomes)

# Create a list split by cell numbers
cell_species <- split(final_df1$scientificName, cell_numbers)

cell_species_biomes <- list()
# Need to split by the endemics to the biome and then the biome
cell_species_biomes <- lapply(names(cell_numbers_biomes), function(x) {
  # Get the endemic species for this biome
  endemic_species <- efg_end[[x]]
  # Filter the df to only include endemic species for this biome
  filtered_df <- final_df1[final_df1$scientificName %in% endemic_species, ]
  # Split
  split(filtered_df$scientificName, cell_numbers_biomes[[x]])
})
names(cell_species_biomes) <- names(cell_numbers_biomes)

# Keep only what exists
cell_species_biomes <- lapply(cell_species_biomes, function(x) {
  x[sapply(x, length) > 0]
})

# Count abundances per species per cell
abundance_list <- lapply(cell_species, function(x) {
  table(x)
})
abundance_list <- abundance_list[!is.na(names(abundance_list))]

# Same for each species in each cell in each biome...
abundance_list_biomes <- lapply(cell_species_biomes, function(x) {
  lapply(x, function(z) {
    table(z)
  })
})
abundance_list_biomes <- lapply(abundance_list_biomes, function(x) {
  x[!is.na(names(x))]
})

# Only the vector of values
abundance_vectors <- lapply(abundance_list, as.vector)
abundance_vectors_biomes <- lapply(abundance_list_biomes, function(x) {
  lapply(x, as.vector)
})

# Measure the sampling coverage (considering N/cell as abundance data type)
sc_df = iNEXT::DataInfo(abundance_vectors, datatype = "abundance")

sc_df_biomes <- lapply(abundance_vectors_biomes, function(x) {
  iNEXT::DataInfo(x, datatype = "abundance")
})
names(sc_df_biomes) <- names(abundance_vectors_biomes)

# How sampling coverage and N are distributed inside each biome?
summary_df <- do.call(rbind, lapply(names(sc_df_biomes), function(x) {
  biome_data <- sc_df_biomes[[x]]
  
  data.frame(
    Biome = x,
    SC_max = max(biome_data$SC, na.rm = TRUE),
    SC_min = min(biome_data$SC, na.rm = TRUE),
    SC_mean = mean(biome_data$SC, na.rm = TRUE),
    SC_median = median(biome_data$SC, na.rm = TRUE),
    n_max = max(biome_data$n, na.rm = TRUE),
    n_min = min(biome_data$n, na.rm = TRUE),
    n_mean = mean(biome_data$n, na.rm = TRUE),
    n_median = median(biome_data$n, na.rm = TRUE),
    n_cells = nrow(biome_data)  # Number of cells in this biome
  )
}))

# Estimates for diversity metrics -----------------------------------------

cl <- parallel::makeCluster(parallel::detectCores()-2)
parallel::clusterEvalQ(cl, library("iNEXT"))
parallel::clusterExport(cl, list("abundance_vectors"), envir = environment())

tmp = parallel::parLapply(cl, 1:length(abundance_vectors), 
                          fun = function(x) {
                            current_abundance <- abundance_vectors[[x]]
                            cell_name <- names(abundance_vectors)[x]
                            
                            tmp1 = iNEXT::iNEXT(current_abundance, 
                                                datatype = "abundance", 
                                                q = c(0, 2),
                                                size = 100, 
                                                endpoint = 100, 
                                                knots = 2, 
                                                se = FALSE)$AsyEst
                            
                            tmp1 <- data.frame(cell_id = cell_name,
                                               Diversity = rownames(tmp1),
                                               tmp1[, 1:3],
                                               row.names = NULL)
                            
                            return(tmp1)
                          })

parallel::stopCluster(cl)

# Convert results to data frame
diversity_df = do.call("rbind.data.frame", tmp)

# Adapting the same code but for biomes separately ------------------------

cl <- parallel::makeCluster(parallel::detectCores()-2)
parallel::clusterEvalQ(cl, library("iNEXT"))

# Process each biome separately
tmp_biomes <- lapply(names(abundance_vectors_biomes), function(biome_name) {
  cat("Processing biome:", biome_name, "\n")
  
  biome_abundance <- abundance_vectors_biomes[[biome_name]]
  parallel::clusterExport(cl, list("biome_abundance"), envir = environment())
  
  # Process each cell in this biome
  tmp_biome <- parallel::parLapply(cl,
                                   1:length(biome_abundance),
                                   fun = function(x) {
                                     current_abundance <- biome_abundance[[x]]
                                     cell_name <- names(biome_abundance)[x]
                                     
                                     tmp1 = iNEXT::iNEXT(current_abundance,
                                                         datatype = "abundance",
                                                         q = c(0, 2),
                                                         size = 100,
                                                         endpoint = 100,
                                                         knots = 2,
                                                         se = FALSE)$AsyEst
                                     
                                     tmp1 <- data.frame(biome = biome_name,
                                                        cell_id = cell_name,
                                                        Diversity = rownames(tmp1),
                                                        tmp1[, 1:3],
                                                        row.names = NULL)
                                     return(tmp1)
                                   }
  )
  do.call(rbind, tmp_biome)
})
names(tmp_biomes) <- efg_names

parallel::stopCluster(cl)

# Combine results from all biomes
diversity_df_biomes <- do.call(rbind, tmp_biomes)

# Getting metrics, finally ------------------------------------------------

# Get only richness and Simpson
rich_df <- diversity_df[grepl("Richness", diversity_df$Diversity), ]
rich_df$cell_id <- as.numeric(rich_df$cell_id)
names(rich_df)[which(names(rich_df) == "Estimator")] <- "S.est"
simp_df <- diversity_df[grepl("Simpson", diversity_df$Diversity), ]
simp_df$cell_id <- as.numeric(simp_df$cell_id)
names(simp_df)[which(names(simp_df) == "Estimator")] <- "Simp.est"

rich_df_biomes <- diversity_df_biomes[grepl("Richness", diversity_df_biomes$Diversity), ]
rich_df_biomes$cell_id <- as.numeric(rich_df_biomes$cell_id)
names(rich_df_biomes)[which(names(rich_df_biomes) == "Estimator")] <- "S.est"
simp_df_biomes <- diversity_df_biomes[grepl("Simpson", diversity_df_biomes$Diversity), ]
simp_df_biomes$cell_id <- as.numeric(simp_df_biomes$cell_id)
names(simp_df_biomes)[which(names(simp_df_biomes) == "Estimator")] <- "Simp.est"

# Get this information back to the raster --------------------------------

cell_id <- as.numeric(names(abundance_vectors))
cell_id_biomes <- lapply(abundance_vectors_biomes, function(x) as.numeric(names(x)))

# Separate what is going to be returned to the raster
sc_df <- data.frame(cell_id = as.numeric(sc_df$Assemblage),
                    n = sc_df$n,
                    S.obs = sc_df$S.obs,
                    Simp.obs = simp_df$Observed,
                    SC = sc_df$SC)

# Get coordinates for these specific cells
cell_coords <- data.frame(cell_id = cell_id,
                          x = terra::xFromCell(my_we_raster, cell_id),
                          y = terra::yFromCell(my_we_raster, cell_id))

# Now the metrics
cell_metrics <- dplyr::left_join(sc_df,
                                 cell_coords,
                                 by = "cell_id")

# Populate new layers
my_we_raster <- c(my_we_raster,
                  terra::rast(my_we_raster, nlyrs = 4))
names(my_we_raster)[(terra::nlyr(my_we_raster)-3):terra::nlyr(my_we_raster)] <- 
  c("SC", "n", "S.obs", "Simp.obs")

my_we_raster[["SC"]][cell_metrics$cell_id] <- cell_metrics$SC
my_we_raster[["n"]][cell_metrics$cell_id] <- cell_metrics$n
my_we_raster[["S.obs"]][cell_metrics$cell_id] <- cell_metrics$S.obs
my_we_raster[["Simp.obs"]][cell_metrics$cell_id] <- cell_metrics$Simp.obs

# Which cells are going to be used for interpolation?
my_we_raster1 <- terra::as.data.frame(my_we_raster, xy = TRUE, cells = TRUE)

# Doing the same for each biome separately --------------------------------

# Separate what is going to be returned to the raster
sc_df_biomes <- lapply(sc_df_biomes, function(x) {
  x$Assemblage <- as.numeric(x$Assemblage)
  return(x)
})

cell_metrics_biomes <- list()

# Iterate through each biome
for(i in names(sc_df_biomes)) {
  
  sc_df_biome <- sc_df_biomes[[i]]
  simp_df_biome <- simp_df_biomes[simp_df_biomes$biome == i, ]
  raster_biome <- my_we_raster_biomes[[i]]
  cell_id_biome <- cell_id_biomes[[i]]
  
  sc_df <- data.frame(cell_id = sc_df_biome$Assemblage,
                      n = sc_df_biome$n,
                      S.obs = sc_df_biome$S.obs,
                      Simp.obs = simp_df_biome$Observed,
                      SC = sc_df_biome$SC)
  
  cell_coords <- data.frame(cell_id = cell_id_biome,
                            x = terra::xFromCell(raster_biome, cell_id_biome),
                            y = terra::yFromCell(raster_biome, cell_id_biome))
  
  cell_metrics_biome <- dplyr::left_join(sc_df, cell_coords, by = "cell_id")
  cell_metrics_biomes[[i]] <- cell_metrics_biome
}

# Populate the rasters now...
raster_df_biomes <- vector("list", length(cell_metrics_biomes))
names(raster_df_biomes) <- names(cell_metrics_biomes)

# Iterate through each biome
for(i in seq_along(cell_metrics_biomes)) {
  
  biome_name <- names(cell_metrics_biomes)[i]
  cell_metrics_biome <- cell_metrics_biomes[[i]]
  raster_biome <- my_we_raster_biomes[[i]]
  
  sc_layer <- terra::rast(raster_biome)
  n_layer <- terra::rast(raster_biome)
  sobs_layer <- terra::rast(raster_biome)
  simp_layer <- terra::rast(raster_biome)
  
  terra::values(sc_layer) <- NA
  terra::values(n_layer) <- NA
  terra::values(sobs_layer) <- NA
  terra::values(simp_layer) <- NA
  
  raster_biome <- c(raster_biome, sc_layer, n_layer, sobs_layer, simp_layer)
  n_layers <- terra::nlyr(raster_biome)
  names(raster_biome)[(n_layers-3):n_layers] <- c("SC", "n", "S.obs", "Simp.obs")
  
  raster_biome[["SC"]][cell_metrics_biome$cell_id] <- cell_metrics_biome$SC
  raster_biome[["n"]][cell_metrics_biome$cell_id] <- cell_metrics_biome$n
  raster_biome[["S.obs"]][cell_metrics_biome$cell_id] <- cell_metrics_biome$S.obs
  raster_biome[["Simp.obs"]][cell_metrics_biome$cell_id] <- cell_metrics_biome$Simp.obs
  
  raster_df_biome <- terra::as.data.frame(raster_biome, xy = TRUE, cells = TRUE) 
  raster_df_biomes[[i]] <- raster_df_biome
}

my_we_raster_biomes1 <- dplyr::bind_rows(raster_df_biomes, .id = "biome")
# Editing names in both dfs
names(my_we_raster_biomes1)[which(names(my_we_raster_biomes1) == "our_efg")] <- "we"
names(my_we_raster1)[which(names(my_we_raster1) == "our_efg")] <- "we"

# Using sample coverage and N cut-offs following Lima et al. 2020 ---------

# Get the quantiles for SC
sc_quantiles <- quantile(my_we_raster1$SC, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)

# Condition 1: between 25 and 50 quantiles of sc and more than 75 occ. (were 100)
cond1 <- my_we_raster1$SC >= sc_quantiles["25%"] & 
  my_we_raster1$SC <= sc_quantiles["50%"] & 
  my_we_raster1$n >= 75

# Condition 2: between 50 and 75 quantiles of sc and more than 35 occ. (were 50)
cond2 <- my_we_raster1$SC >= sc_quantiles["50%"] & 
  my_we_raster1$SC <= sc_quantiles["75%"] & 
  my_we_raster1$n >= 35

# Condition 3: more than 75 quantiles of sc and more than 30 occ.
cond3 <- my_we_raster1$SC >= sc_quantiles["75%"] & 
  my_we_raster1$n >= 30

# Tag rows that meet any condition
my_we_raster1$use_this <- cond1 | cond2 | cond3

# How many cells and the description of the cells per se
sum(my_we_raster1$use_this[my_we_raster1$use_this %in% TRUE]) /
  nrow(my_we_raster1) # 37% of cells

# Other metrics for n of occurrences and sampling coverage
summary(my_we_raster1$n[my_we_raster1$use_this %in% TRUE])
summary(my_we_raster1$SC[my_we_raster1$use_this %in% TRUE])

# Changing one name
names(my_we_raster1)[which(names(my_we_raster1) == "our_efg")] <- "we"

# I am not rarefying biomes separately... --------------------------------

# Interpolating diversity metrics -----------------------------------------

# Grid to predict the metrics
my_we_grid <- terra::rast(extent = terra::ext(my_we_raster),
                          resolution = 0.01,
                          crs = terra::crs(my_we_raster))
my_we_grid <- terra::as.data.frame(my_we_grid, xy = TRUE, na.rm = FALSE)
my_we_grid$lyr.1 <- NULL

# Using only the minimum SC points
my_we_pts_sp <- my_we_raster1[my_we_raster1$use_this %in% TRUE, ]
sp::coordinates(my_we_pts_sp) <- ~x+y

# Interpolating weighted endemism -----------------------------------------

# Make variogram
v <- automap::autofitVariogram(log(we) ~ 1, my_we_pts_sp)
vario <- v$var_model
spp_corr_we <- vario$range[2]
krig_mod <- gstat::gstat(id = "we",
                         formula = log(we) ~ 1, # Interpolating log-transformed values
                         locations = ~x+y,
                         data = my_we_raster1[my_we_raster1$use_this %in% TRUE, ],
                         model = gstat::vgm(model = as.character(vario$model)[2],
                                            psill = vario[2, 2],
                                            range = vario[2, 3],
                                            kappa = vario[2, 4]),
                         nmax = 30)

# Predict the WE model in the grid
my_we_grid$krig.we <- predict(krig_mod, my_we_grid)$we.pred

# Interpolating observed species richness --------------------------------

# Make variogram
v <- automap::autofitVariogram(log(S.obs) ~ 1, my_we_pts_sp)
vario <- v$var_model
spp_corr_sr <- vario$range[2]
krig_mod <- gstat::gstat(id = "S.obs",
                         formula = log(S.obs) ~ 1, # Interpolating log-transformed values
                         locations = ~x+y,
                         data = my_we_raster1[my_we_raster1$use_this %in% TRUE, ],
                         model = gstat::vgm(model = as.character(vario$model)[2],
                                            psill = vario[2, 2],
                                            range = vario[2, 3],
                                            kappa = vario[2, 4]),
                         nmax = 30)

# Predict the observed species richness model in the grid
my_we_grid$krig.s.obs <- predict(krig_mod, my_we_grid)$S.obs.pred

# Interpolating observed Simpson diversity --------------------------------

# Make variogram
v <- automap::autofitVariogram(log(Simp.obs) ~ 1, my_we_pts_sp)
vario <- v$var_model
spp_corr_simp <- vario$range[2]
krig_mod <- gstat::gstat(id = "Simp.obs",
                         formula = log(Simp.obs) ~ 1, # Interpolating log-transformed values
                         locations = ~x+y,
                         data = my_we_raster1[my_we_raster1$use_this %in% TRUE, ],
                         model = gstat::vgm(model = as.character(vario$model)[2],
                                            psill = vario[2, 2],
                                            range = vario[2, 3],
                                            kappa = vario[2, 4]),
                         nmax = 30)

# Predict the observed species richness model in the grid
my_we_grid$krig.simp.obs <- predict(krig_mod, my_we_grid)$Simp.obs.pred

# Convert to raster for cropping, masking, and plotting
my_we_grid <- terra::rast(x = my_we_grid, crs = "epsg:4326")

# Crop and mask
my_int <- terra::crop(my_we_grid, tropical_america)
my_int <- terra::mask(my_int, tropical_america)

# Save this result
terra::writeRaster(my_int, here::here("data", "derived-data", 
                                      "maps", "interpolated_maps1km.tiff"),
                   overwrite = TRUE)



# DOING THE SAME THING BUT FOR EACH BIOME SEPARATELY ----------------------

# Interpolating diversity metrics -----------------------------------------

# Biomes map
biomes_map <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp"))
biomes_map <- biomes_map[biomes_map$Morrone_re %in% "Neotropical", ]

# Filter only the ones with endemic species
biomes_map <- biomes_map[biomes_map$our_efg %in% names(efg_end), ]

# Looping for each biome that contains more than 200 endemic species
# Otherwise interpolation maps become unfeasible to make
biome_names <- names(efg_end)[sapply(efg_end, function(x) length(x) >= 200)]
my_we_grid_biomes <- vector("list", length(biome_names))
names(my_we_grid_biomes) <- biome_names

for(i in names(my_we_grid_biomes)){
  cat("Interpolating data from", i, "\n")
  
  # Grid to predict the metrics
  # Rasterize the extent of the 
  my_we_grid <- terra::rast(extent = terra::ext(biomes_map[biomes_map$our_efg %in% i, ]),
                            resolution = 0.01,
                            crs = terra::crs(biomes_map))
  
  my_we_pts_sp <- my_we_raster_biomes1[my_we_raster_biomes1$biome %in% i, ]
  my_we_pts_sp <- terra::vect(my_we_pts_sp, geom = c("x", "y"), crs = "epsg:4326")
  my_we_pts_sp$log.we <- log(my_we_pts_sp$we)
  
  x <- terra::interpIDW(x = my_we_grid, 
                        y = my_we_pts_sp, 
                        field = "log.we",
                        smooth = 2,
                        power = 2, 
                        radius = 2)
  
  # Crop, mask, and save
  x_clipped <- terra::crop(x, biomes_map[biomes_map$our_efg %in% i, ])
  x_clipped <- terra::mask(x, biomes_map[biomes_map$our_efg %in% i, ])
  my_we_grid_biomes[[i]] <- x_clipped
}

# Save separately
filenames <- paste0("interpolated_maps_biomes_1km_", 
                    make.names(names(my_we_grid_biomes)), ".tiff")

for (i in seq_along(my_we_grid_biomes)) {
  terra::writeRaster(my_we_grid_biomes[[i]], 
                     here::here("data", "derived-data", "maps", filenames[i]),
                     overwrite = TRUE)
}