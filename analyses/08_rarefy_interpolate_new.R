# Rarefaction and interpolation -------------------------------------------

devtools::load_all()

# Load data
final_df <- readRDS(here::here("data", "derived-data", "rds", "final_df.rds"))
final_df$decimalLatitude.new1 <- as.numeric(final_df$decimalLatitude.new1)
final_df$decimalLongitude.new1 <- as.numeric(final_df$decimalLongitude.new1)
we_map_area <- readRDS(here::here("data", "derived-data", "rds", "we_map_area_50km.rds"))
we_map_area_biomes <- readRDS(here::here("data", "derived-data", "rds", "we_map_area_biomes_50km.rds"))

# Read null
read_path <- here::here("data", "derived-data", "rds")
read_name1 <- "we_null_TA_all.rds"
read_name2 <- "we_null_TA_biomes.rds"

# Load null
we_null_test <- readRDS(file.path(read_path, read_name1))
we_null_test_biomes <- readRDS(file.path(read_path, read_name2))

# Load other things
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

# To remove categories...get only values
strip_categories <- function(r) {
  vals <- terra::values(r)
  r_new <- terra::rast(r)
  terra::values(r_new) <- vals
  return(r_new)
}

# Add null-model rasters directly to the raster stacks
# For the whole area - add all null rasters
my_we_raster <- c(my_we_raster, 
                  strip_categories(terra::rast(we_null_test$out.continuous.raster)),
                  strip_categories(terra::rast(we_null_test$P.above.raster)),
                  strip_categories(terra::rast(we_null_test$out.above.below.raster)))
names(my_we_raster) <- c("we",
                         "we_null_out_continuous", 
                         "we_null_P_above", 
                         "we_null_out_above_below")

# Add null rasters to each biome
for(i in names(my_we_raster_biomes)) {
  # Get the null model for this biome
  null_biome <- we_null_test_biomes[[i]]
  # Add the null rasters
  my_we_raster_biomes[[i]] <- c(my_we_raster_biomes[[i]],
                                strip_categories(terra::rast(null_biome$out.continuous.raster)),
                                strip_categories(terra::rast(null_biome$P.above.raster)),
                                strip_categories(terra::rast(null_biome$out.above.below.raster)))
  # Rename
  names(my_we_raster_biomes[[i]]) <- c("we",
                                       "we_null_out_continuous", 
                                       "we_null_P_above", 
                                       "we_null_out_above_below")
}



##### Convert rasters to data frames #####



# Tropical America
my_we_df <- as.data.frame(my_we_raster, xy = TRUE, na.rm = FALSE)
my_we_df$cell_id <- terra::cellFromXY(my_we_raster, my_we_df[, c("x", "y")])

# Biomes
my_we_df_biomes <- list()
for(biome_name in names(my_we_raster_biomes)) {
  df <- as.data.frame(my_we_raster_biomes[[biome_name]], xy = TRUE, na.rm = FALSE)
  df$cell_id <- terra::cellFromXY(my_we_raster_biomes[[biome_name]], df[, c("x", "y")])
  df$biome <- biome_name
  my_we_df_biomes[[biome_name]] <- df
}



# Convert occ points to spatial points
final_df1 <- terra::vect(final_df[final_df$is_realm_predominant %in% FALSE 
                                  & final_df$is_realm_transitional %in% FALSE, ], 
                         geom = c("decimalLongitude.new1", "decimalLatitude.new1"), 
                         crs = "epsg:4326")
# Get cell numbers for all points
cell_numbers <- terra::cellFromXY(my_we_raster, terra::crds(final_df1))
# Create a list split by cell numbers
cell_species <- split(final_df1$scientificName, cell_numbers)
# Do the same for biomes, using only the endemics to each biome; species names
# and cell numbers come from the same points (those inside the biome)
cell_species_biomes <- lapply(names(my_we_raster_biomes), function(x) {
  biome_raster <- my_we_raster_biomes[[x]]
  pts <- final_df1[final_df1$scientificName %in% efg_end[[x]], ]
  pts <- terra::intersect(pts, terra::as.polygons(biome_raster[["we"]]))
  cells <- terra::cellFromXY(biome_raster, terra::crds(pts))
  split(pts$scientificName[!is.na(cells)], cells[!is.na(cells)])
})
names(cell_species_biomes) <- names(my_we_raster_biomes)
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


# Calculating coverage ----------------------------------------------------

##### For tropical America #####
# Seed for the random orders of the accumulation curves (slopes are reproducible)
set.seed(2026)
coverage_results <- lapply(names(cell_species), function(cell_id) {
  species_vector <- cell_species[[cell_id]]
  metrics <- cell_coverage(species_vector, base.n = 20, base.richness = 20, perms = 999)
  # Return as list with cell_id
  return(c(list(cell_id = as.numeric(cell_id)), metrics))
})
coverage_df <- dplyr::bind_rows(coverage_results)
coverage_df$interpolate <- ifelse(is.na(coverage_df$slope), FALSE, TRUE)

# -----------------------

##### For each biome #####
set.seed(2026)
coverage_results_biomes <- lapply(names(cell_species_biomes), function(biome_name) {
  cat("  Processing", biome_name, "...\n")
  biome_species <- cell_species_biomes[[biome_name]]
  
  biome_coverage <- lapply(names(biome_species), function(cell_id) {
    species_vector <- biome_species[[cell_id]]
    metrics <- cell_coverage(species_vector, base.n = 5, base.richness = 5, perms = 999)
    # Return as list with cell_id
    return(c(list(cell_id = as.numeric(cell_id)), metrics))
  })
  
  # Convert to data frame and add biome column
  biome_coverage_df <- dplyr::bind_rows(biome_coverage)
  biome_coverage_df$biome <- biome_name
  
  return(biome_coverage_df)
})

coverage_df_biomes <- dplyr::bind_rows(coverage_results_biomes)
coverage_df_biomes$interpolate <- ifelse(is.na(coverage_df_biomes$slope), FALSE, TRUE)


# Before merging, remove cells without data 
my_we_df1 <- my_we_df[!is.na(my_we_df$we), ]
my_we_df_biomes1 <- dplyr::bind_rows(my_we_df_biomes)
my_we_df_biomes1 <- my_we_df_biomes1[!is.na(my_we_df_biomes1$we), ]

# Bind
my_we_df1 <- dplyr::left_join(my_we_df1, coverage_df, by = "cell_id")
my_we_df_biomes1 <- dplyr::left_join(my_we_df_biomes1, coverage_df_biomes,
                                     by = c("cell_id", "biome"))




# # OLD APPROACH: IGNORE ----------------------------------------------------
# 
# # Measure the sampling coverage (considering N/cell as abundance data type)
# sc_df = iNEXT::DataInfo(abundance_vectors, datatype = "abundance")
# sc_df_biomes <- lapply(abundance_vectors_biomes, function(x) {
#   iNEXT::DataInfo(x, datatype = "abundance")
# })
# names(sc_df_biomes) <- names(abundance_vectors_biomes)
# # How sampling coverage and N are distributed inside each biome?
# summary_df <- do.call(rbind, lapply(names(sc_df_biomes), function(x) {
#   biome_data <- sc_df_biomes[[x]]
#   
#   data.frame(
#     Biome = x,
#     SC_max = max(biome_data$SC, na.rm = TRUE),
#     SC_min = min(biome_data$SC, na.rm = TRUE),
#     SC_mean = mean(biome_data$SC, na.rm = TRUE),
#     SC_median = median(biome_data$SC, na.rm = TRUE),
#     n_max = max(biome_data$n, na.rm = TRUE),
#     n_min = min(biome_data$n, na.rm = TRUE),
#     n_mean = mean(biome_data$n, na.rm = TRUE),
#     n_median = median(biome_data$n, na.rm = TRUE),
#     n_cells = nrow(biome_data)  # Number of cells in this biome
#   )
# }))
# # Estimates for diversity metrics -----------------------------------------
# cl <- parallel::makeCluster(parallel::detectCores()-2)
# parallel::clusterEvalQ(cl, library("iNEXT"))
# parallel::clusterExport(cl, list("abundance_vectors"), envir = environment())
# tmp = parallel::parLapply(cl, 1:length(abundance_vectors), 
#                           fun = function(x) {
#                             current_abundance <- abundance_vectors[[x]]
#                             cell_name <- names(abundance_vectors)[x]
#                             
#                             tmp1 = iNEXT::iNEXT(current_abundance, 
#                                                 datatype = "abundance", 
#                                                 q = c(0, 2),
#                                                 size = 100, 
#                                                 endpoint = 100, 
#                                                 knots = 2, 
#                                                 se = FALSE)$AsyEst
#                             
#                             tmp1 <- data.frame(cell_id = cell_name,
#                                                Diversity = rownames(tmp1),
#                                                tmp1[, 1:3],
#                                                row.names = NULL)
#                             
#                             return(tmp1)
#                           })
# parallel::stopCluster(cl)
# # Convert results to data frame
# diversity_df = do.call("rbind.data.frame", tmp)
# # Adapting the same code but for biomes separately ------------------------
# cl <- parallel::makeCluster(parallel::detectCores()-2)
# parallel::clusterEvalQ(cl, library("iNEXT"))
# # Process each biome separately
# tmp_biomes <- lapply(names(abundance_vectors_biomes), function(biome_name) {
#   cat("Processing biome:", biome_name, "\n")
#   
#   biome_abundance <- abundance_vectors_biomes[[biome_name]]
#   parallel::clusterExport(cl, list("biome_abundance"), envir = environment())
#   
#   # Process each cell in this biome
#   tmp_biome <- parallel::parLapply(cl,
#                                    1:length(biome_abundance),
#                                    fun = function(x) {
#                                      current_abundance <- biome_abundance[[x]]
#                                      cell_name <- names(biome_abundance)[x]
#                                      
#                                      tmp1 = iNEXT::iNEXT(current_abundance,
#                                                          datatype = "abundance",
#                                                          q = c(0, 2),
#                                                          size = 100,
#                                                          endpoint = 100,
#                                                          knots = 2,
#                                                          se = FALSE)$AsyEst
#                                      
#                                      tmp1 <- data.frame(biome = biome_name,
#                                                         cell_id = cell_name,
#                                                         Diversity = rownames(tmp1),
#                                                         tmp1[, 1:3],
#                                                         row.names = NULL)
#                                      return(tmp1)
#                                    }
#   )
#   do.call(rbind, tmp_biome)
# })
# names(tmp_biomes) <- efg_names
# parallel::stopCluster(cl)
# # Combine results from all biomes
# diversity_df_biomes <- do.call(rbind, tmp_biomes)
# # Getting metrics, finally ------------------------------------------------
# # Get only richness and Simpson
# rich_df <- diversity_df[grepl("Richness", diversity_df$Diversity), ]
# rich_df$cell_id <- as.numeric(rich_df$cell_id)
# names(rich_df)[which(names(rich_df) == "Estimator")] <- "S.est"
# simp_df <- diversity_df[grepl("Simpson", diversity_df$Diversity), ]
# simp_df$cell_id <- as.numeric(simp_df$cell_id)
# names(simp_df)[which(names(simp_df) == "Estimator")] <- "Simp.est"
# rich_df_biomes <- diversity_df_biomes[grepl("Richness", diversity_df_biomes$Diversity), ]
# rich_df_biomes$cell_id <- as.numeric(rich_df_biomes$cell_id)
# names(rich_df_biomes)[which(names(rich_df_biomes) == "Estimator")] <- "S.est"
# simp_df_biomes <- diversity_df_biomes[grepl("Simpson", diversity_df_biomes$Diversity), ]
# simp_df_biomes$cell_id <- as.numeric(simp_df_biomes$cell_id)
# names(simp_df_biomes)[which(names(simp_df_biomes) == "Estimator")] <- "Simp.est"
# # Get this information back to the raster --------------------------------
# cell_id <- as.numeric(names(abundance_vectors))
# cell_id_biomes <- lapply(abundance_vectors_biomes, function(x) as.numeric(names(x)))
# # Separate what is going to be returned to the raster
# sc_df <- data.frame(cell_id = as.numeric(sc_df$Assemblage),
#                     n = sc_df$n,
#                     S.obs = sc_df$S.obs,
#                     Simp.obs = simp_df$Observed,
#                     SC = sc_df$SC)
# # Get coordinates for these specific cells
# cell_coords <- data.frame(cell_id = cell_id,
#                           x = terra::xFromCell(my_we_raster, cell_id),
#                           y = terra::yFromCell(my_we_raster, cell_id))
# # Now the metrics
# cell_metrics <- dplyr::left_join(sc_df,
#                                  cell_coords,
#                                  by = "cell_id")
# # Populate new layers
# my_we_raster <- c(my_we_raster,
#                   terra::rast(my_we_raster, nlyrs = 4))
# names(my_we_raster)[(terra::nlyr(my_we_raster)-3):terra::nlyr(my_we_raster)] <- 
#   c("SC", "n", "S.obs", "Simp.obs")
# my_we_raster[["SC"]][cell_metrics$cell_id] <- cell_metrics$SC
# my_we_raster[["n"]][cell_metrics$cell_id] <- cell_metrics$n
# my_we_raster[["S.obs"]][cell_metrics$cell_id] <- cell_metrics$S.obs
# my_we_raster[["Simp.obs"]][cell_metrics$cell_id] <- cell_metrics$Simp.obs
# # Which cells are going to be used for interpolation?
# my_we_raster1 <- terra::as.data.frame(my_we_raster, xy = TRUE, cells = TRUE)
# 
# # Doing the same for each biome separately --------------------------------
# # Separate what is going to be returned to the raster
# sc_df_biomes <- lapply(sc_df_biomes, function(x) {
#   x$Assemblage <- as.numeric(x$Assemblage)
#   return(x)
# })
# cell_metrics_biomes <- list()
# # Iterate through each biome
# for(i in names(sc_df_biomes)) {
#   
#   sc_df_biome <- sc_df_biomes[[i]]
#   simp_df_biome <- simp_df_biomes[simp_df_biomes$biome == i, ]
#   raster_biome <- my_we_raster_biomes[[i]]
#   cell_id_biome <- cell_id_biomes[[i]]
#   
#   sc_df <- data.frame(cell_id = sc_df_biome$Assemblage,
#                       n = sc_df_biome$n,
#                       S.obs = sc_df_biome$S.obs,
#                       Simp.obs = simp_df_biome$Observed,
#                       SC = sc_df_biome$SC)
#   
#   cell_coords <- data.frame(cell_id = cell_id_biome,
#                             x = terra::xFromCell(raster_biome, cell_id_biome),
#                             y = terra::yFromCell(raster_biome, cell_id_biome))
#   
#   cell_metrics_biome <- dplyr::left_join(sc_df, cell_coords, by = "cell_id")
#   cell_metrics_biomes[[i]] <- cell_metrics_biome
# }
# # Populate the rasters now...
# raster_df_biomes <- vector("list", length(cell_metrics_biomes))
# names(raster_df_biomes) <- names(cell_metrics_biomes)
# # Iterate through each biome
# for(i in seq_along(cell_metrics_biomes)) {
#   
#   biome_name <- names(cell_metrics_biomes)[i]
#   cell_metrics_biome <- cell_metrics_biomes[[i]]
#   raster_biome <- my_we_raster_biomes[[i]]
#   
#   sc_layer <- terra::rast(raster_biome$we)
#   n_layer <- terra::rast(raster_biome$we)
#   sobs_layer <- terra::rast(raster_biome$we)
#   simp_layer <- terra::rast(raster_biome$we)
#   
#   terra::values(sc_layer) <- NA
#   terra::values(n_layer) <- NA
#   terra::values(sobs_layer) <- NA
#   terra::values(simp_layer) <- NA
#   
#   raster_biome <- c(raster_biome, sc_layer, n_layer, sobs_layer, simp_layer)
#   n_layers <- terra::nlyr(raster_biome)
#   names(raster_biome)[(n_layers-3):n_layers] <- c("SC", "n", "S.obs", "Simp.obs")
#   
#   raster_biome[["SC"]][cell_metrics_biome$cell_id] <- cell_metrics_biome$SC
#   raster_biome[["n"]][cell_metrics_biome$cell_id] <- cell_metrics_biome$n
#   raster_biome[["S.obs"]][cell_metrics_biome$cell_id] <- cell_metrics_biome$S.obs
#   raster_biome[["Simp.obs"]][cell_metrics_biome$cell_id] <- cell_metrics_biome$Simp.obs
#   
#   # Convert to dataframe - null rasters are already included from earlier!
#   raster_df_biome <- terra::as.data.frame(raster_biome, xy = TRUE, cells = TRUE)
#   
#   # ---- REMOVED: The left_join with null_df_biome is no longer needed
#   # because the null rasters were already added to my_we_raster_biomes[[i]]
#   # earlier in the code and are now included in raster_biome
#   
#   raster_df_biomes[[i]] <- raster_df_biome
# }
# my_we_raster_biomes1 <- dplyr::bind_rows(raster_df_biomes, .id = "biome")
# 
# # END OF OLD APPROACH -----------------------------------------------------





# Interpolating weighted endemism -----------------------------------------

# Subset only well-sampled cells based on the slope parameter
# The smaller the slope, the more asymptotic is the predicted curve
# First, remove NAs from here too...these are interpolate FALSE
my_we_df2 <- my_we_df1[my_we_df1$interpolate %in% TRUE, ]
my_we_df_biomes2 <- my_we_df_biomes1[my_we_df_biomes1$interpolate %in% TRUE, ]

# tropical America
# ta_quants <- quantile(my_we_df2$slope, probs = seq(0, 1, 0.05), na.rm = TRUE)
my_we_df3 <- my_we_df2[my_we_df2$slope < 0.5, ]
cat("we are using", 
    round(100*dim(my_we_df3)[1]/(dim(my_we_df1)[1]), 2), 
    "percent of the cells") 

# Biomes separately
# ta_quants <- quantile(my_we_df2$slope, probs = seq(0, 1, 0.05), na.rm = TRUE)
my_we_df_biomes3 <- my_we_df_biomes2[my_we_df_biomes2$slope < 0.9, ]
# my_we_df_biomes3 <- my_we_df_biomes2
cat("we are using", 
    round(100*dim(my_we_df_biomes3)[1]/(dim(my_we_df_biomes1)[1]), 2), 
    "percent of the cells") 


# # REMOVING FISHER'S HERE TOO ----------------------------------------------
# 
# # Sample coverage from Fisher's alpha -------------------------------------
# ab_vect_new <- abundance_vectors[names(abundance_vectors) %in% my_we_pts$cell]
# # Removing cells that have only singletons...
# ab_vect_new <- ab_vect_new[!sapply(ab_vect_new, function(x) 
#   all(x == 1))]
# 
# # Find the cell with most occurrences
# total_N <- sapply(ab_vect_new, sum)
# cell_max_name <- names(total_N)[which.max(total_N)]
# cell_max_N <- max(total_N)
# cell_max_data <- ab_vect_new[[cell_max_name]]
# 
# # Fisher's alpha for each cell
# cell_alpha <- sapply(ab_vect_new, vegan::fisher.alpha)
# 
# # Expected richness for each cell using its alpha and targeted at max occurrences
# expected_richness <- sapply(seq_along(ab_vect_new), function(x) {
#   alpha <- cell_alpha[x]
#   N_target <- cell_max_N
#   S_expected <- alpha * log(1 + N_target / alpha)
#   return(S_expected)
# })
# names(expected_richness) <- names(ab_vect_new)
# 
# # Completeness from the relation of observed/expected
# observed_richness <- sapply(ab_vect_new, length)
# completeness <- observed_richness / expected_richness
# names(completeness) <- names(ab_vect_new)
# 
# # Bind
# results_df <- data.frame(cell = as.numeric(names(ab_vect_new)),
#                          fisher.alpha = round(cell_alpha, 0),
#                          S.exp = expected_richness)
# 
# # Join to my_we_pts
# my_we_pts1 <- dplyr::left_join(my_we_pts, results_df, by = "cell")
# # Remove NA (spp = N), fisher's break it down. Is there a fix?
# # Around 1200 cells...
# my_we_pts1 <- my_we_pts1[!is.na(my_we_pts1$fisher.alpha), ]
# # Calculate completeness
# my_we_pts1$fisher.completeness <- my_we_pts1$S.obs / my_we_pts1$S.exp
# 
# 
# # Does alpha stabilizes?
# plot(my_we_pts1$n, my_we_pts1$fisher.alpha,
#      xlab = "Sample size (N)",
#      ylab = "Fisher's alpha",
#      las = 1,
#      pch = 19, 
#      cex = 0.5)
# lines(lowess(my_we_pts1$n, my_we_pts1$fisher.alpha),
#       col = "red", lwd = 2)
# text(x = 10000, y = 100,
#      labels = paste("Correlation: ",
#                     round(cor(my_we_pts1$n, my_we_pts1$fisher.alpha), 2)))
# 
# # Fisher's x Chao's
# plot(my_we_pts1$fisher.completeness,
#      my_we_pts1$SC,
#      xlab = "Fisher's completeness (Observed/Expected)",
#      ylab = "Sample coverage (Chao's)",
#      las = 1,
#      pch = 19,
#      cex = 0.5)
# lines(lowess(x = my_we_pts1$fisher.completeness,
#              y = my_we_pts1$SC), lwd = 2, col = "red")
# text(x = 0.9, y = 0.1, 
#      labels = paste("Correlation: ", 
#                     round(cor(my_we_pts1$fisher.completeness, 
#                               my_we_pts1$SC), 2)))
# 
# 
# # Keeping only a subset of cells for interpolation
# fisher.quant <- quantile(my_we_pts1$fisher.completeness,
#                          probs = seq(0, 1, 0.1))
# 
# # Trying 40%, which is more than 10% of completeness...
# my_we_pts2 <- my_we_pts1[my_we_pts1$fisher.completeness > fisher.quant[[5]], ]


# Prediction grid to be used
# (without the text column "model", which cannot be a raster layer)
my_we_raster_new <- terra::rast(my_we_df1[, names(my_we_df1) != "model"], type = "xyz", crs = "epsg:4326")
# 
# my_we_grid <- terra::rast(extent = terra::ext(my_we_raster_new),
#                           resolution = 0.5,
#                           crs = terra::crs(my_we_raster_new))
# my_we_grid <- c(my_we_grid, my_we_raster_new[["we"]], my_we_raster_new[["S_obs"]])
my_we_grid <- terra::as.data.frame(my_we_raster_new, xy = TRUE, na.rm = FALSE)

# Kriging uses distances in metres: cell centres are also projected to the
# same equal-area projection of the maps (and of Assessments); x and y stay in
# lon/lat to rebuild the rasters
proj_m <- "+proj=aea +lat_1=10 +lat_2=-20 +lat_0=-5 +lon_0=-65 +datum=WGS84 +units=m +no_defs"
my_we_grid[, c("xm", "ym")] <- terra::project(as.matrix(my_we_grid[, c("x", "y")]),
                                              from = "epsg:4326", to = proj_m)

# Convert points to spatial object
my_we_pts_sp <- my_we_df3
my_we_pts_sp[, c("xm", "ym")] <- terra::project(as.matrix(my_we_pts_sp[, c("x", "y")]),
                                                from = "epsg:4326", to = proj_m)
sp::coordinates(my_we_pts_sp) <- ~xm + ym

# # Interpolate N across the whole grid
# v_n <- automap::autofitVariogram(log(N) ~ 1, my_we_pts_sp)
# vario_n <- v_n$var_model
# 
# krig_mod_n <- gstat::gstat(id = "N",
#                            formula = log(N) ~ 1,
#                            locations = ~x + y,
#                            data = as.data.frame(my_we_pts_sp),
#                            model = vario_n,
#                            nmax = 30)
# n_pred_log <- predict(krig_mod_n, my_we_grid)$N.pred
# 
# # Back-transform from log scale
# n_pred_raw <- exp(n_pred_log)
# 
# # Fill ONLY the NA cells in the grid
# my_we_grid$N_interpolated <- my_we_grid$N
# my_we_grid$N_interpolated[is.na(my_we_grid$N_interpolated)] <-
#   n_pred_raw[is.na(my_we_grid$N_interpolated)]

# Interpolating weighted endemism finally
v_we <- automap::autofitVariogram(log(we) ~ 1, my_we_pts_sp)
vario_we <- v_we$var_model
krig_mod <- gstat::gstat(id = "we",
                         formula = log(we) ~ 1,
                         locations = ~xm + ym,
                         data = as.data.frame(my_we_pts_sp),
                         model = vario_we,
                         nmax = 30)

# Back to WE units with exp(pred), i.e. the median. The log-normal mean,
# exp(pred + var/2), was tested and moved ~40% of the top 10% cells to poorly
# sampled cells (high kriging variance), so it is not used; the log-scale
# kriging variance is kept as an uncertainty layer
krig_we <- predict(krig_mod, my_we_grid)
my_we_grid$krig.we <- exp(krig_we$we.pred)
my_we_grid$krig.we.var <- krig_we$we.var

# # New pred grid with N_interpolated to be used
# my_we_grid_for_we <- my_we_grid
# my_we_grid_for_we$N <- my_we_grid_for_we$N_interpolated
# my_we_grid_for_we$krig.we <- predict(krig_mod, my_we_grid_for_we)$we.pred


# Interpolating observed species richness --------------------------------

# Interpolate richness across the whole grid
v_rich <- automap::autofitVariogram(log(S_obs) ~ 1, my_we_pts_sp)
vario_rich <- v_rich$var_model
krig_mod_rich <- gstat::gstat(id = "S_obs",
                           formula = log(S_obs) ~ 1,
                           locations = ~xm + ym,
                           data = as.data.frame(my_we_pts_sp),
                           model = vario_rich,
                           nmax = 30)

# Back to number of species, also as the median
krig_rich <- predict(krig_mod_rich, my_we_grid)
my_we_grid$krig.s.obs <- exp(krig_rich$S_obs.pred)
my_we_grid$krig.s.obs.var <- krig_rich$S_obs.var


# ##### GAM + Kriging the residuals while considering n #####
# library(mgcv)
# 
# my_we_pts_df <- as.data.frame(my_we_pts_sp)
# m <- gam(log(we) ~ s(N), data = my_we_pts_df)
# summary(m)
# 
# my_we_pts_sp$we_residuals <- residuals(m)
# 
# v <- automap::autofitVariogram(we_residuals ~ 1, my_we_pts_sp)
# vario <- v$var_model
# 
# krig_mod <- gstat::gstat(id = "we_residuals",
#                          formula = we_residuals ~ 1,
#                          locations = ~x + y,
#                          data = as.data.frame(my_we_pts_sp),
#                          model = vario,
#                          nmax = 30)
# 
# krig_result <- predict(krig_mod, my_we_grid)
# my_we_grid$krig.we_residual     <- krig_result$we_residuals.pred
# my_we_grid$krig.we_residual_var <- krig_result$we_residuals.var
# 
# # --- CHANGED: keep the GAM's standard error too, not just the fitted value ---
# gam_result <- predict(m, newdata = my_we_grid, se.fit = TRUE)
# my_we_grid$gam_pred <- gam_result$fit
# my_we_grid$gam_se   <- gam_result$se.fit
# 
# # Combine trend + residual (still on log scale, unchanged)
# my_we_grid$krig.we_gam <- my_we_grid$gam_pred + my_we_grid$krig.we_residual
# 
# # --- NEW: total prediction variance (GAM trend uncertainty + kriging residual uncertainty) ---
# # Treated as independent sources of error, which is a standard simplifying
# # assumption in this kind of two-stage regression-kriging correction.
# my_we_grid$total_var <- my_we_grid$gam_se^2 + my_we_grid$krig.we_residual_var
# 
# # --- NEW: bias-corrected back-transformation (log-normal / smearing correction) ---
# my_we_grid$we_gam_final_corrected <- exp(my_we_grid$krig.we_gam + my_we_grid$total_var / 2)

my_int <- terra::rast(my_we_grid[, !names(my_we_grid) %in% c("xm", "ym")],
                      type = "xyz", crs = "epsg:4326")
my_int <- terra::crop(terra::mask(my_int, tropical_america), tropical_america)

# Save this result (0.5 degree cells, ~50 km)
terra::writeRaster(my_int, here::here("data", "derived-data",
                                      "maps", "interpolated_maps_50km.tiff"),
                   overwrite = TRUE)



# DOING THE SAME THING BUT FOR EACH BIOME SEPARATELY ----------------------

# Interpolating diversity metrics -----------------------------------------

# Biomes map
biomes_map <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp"))
biomes_map <- biomes_map[biomes_map$Morrone_re %in% "Neotropical", ]

# Filter only the ones with endemic species
biomes_map <- biomes_map[biomes_map$our_efg %in% names(efg_end), ]

# Only the top biomes with more species
biome_names <- names(efg_end)[sapply(efg_end, function(x) length(x) >= 200)]
# Number of endemic species per biome and which biomes were interpolated. Same
# filters used to build end_per_efg in 08_calculate_wee (no biome- or
# realm-predominant/transitional records), so the counts match efg_end
end_per_biome <- final_df[final_df$is_biome_predominant %in% FALSE
                          & final_df$is_biome_transitional %in% FALSE
                          & final_df$is_realm_predominant %in% FALSE
                          & final_df$is_realm_transitional %in% FALSE, ]
end_per_biome <- aggregate(scientificName ~ efg_endemic, data = end_per_biome,
                           FUN = function(x) length(unique(x)))
names(end_per_biome) <- c("efg_endemic", "n_endemic_species")
end_per_biome$interpolated <- end_per_biome$efg_endemic %in% biome_names
writexl::write_xlsx(end_per_biome, here::here("outputs", "tables", "endemic_species_per_efg.xlsx"))
my_we_grid_biomes <- vector("list", length(biome_names))
names(my_we_grid_biomes) <- biome_names

for(i in names(my_we_grid_biomes)){
  cat("Interpolating data from", i, "\n")
  biome_poly <- biomes_map[biomes_map$our_efg %in% i, ]

  # Grid to predict the metrics: the same 0.5 degree cells of the WE maps over
  # the biome extent, with centres also in proj_m for the kriging
  my_we_grid_biome <- terra::crop(terra::rast(my_we_raster), biome_poly, snap = "out")
  my_we_grid_biome <- as.data.frame(terra::xyFromCell(my_we_grid_biome,
                                                      seq_len(terra::ncell(my_we_grid_biome))))
  my_we_grid_biome[, c("xm", "ym")] <- terra::project(as.matrix(my_we_grid_biome[, c("x", "y")]),
                                                      from = "epsg:4326", to = proj_m)
  my_we_pts_sp <- my_we_df_biomes3[my_we_df_biomes3$biome %in% i, ]
  my_we_pts_sp[, c("xm", "ym")] <- terra::project(as.matrix(my_we_pts_sp[, c("x", "y")]),
                                                  from = "epsg:4326", to = proj_m)

  sp::coordinates(my_we_pts_sp) <- ~xm + ym
  v_we <- automap::autofitVariogram(log(we) ~ 1, my_we_pts_sp)
  vario_we <- v_we$var_model
  krig_mod <- gstat::gstat(id = "we",
                           formula = log(we) ~ 1,
                           locations = ~xm + ym,
                           data = as.data.frame(my_we_pts_sp),
                           model = vario_we,
                           nmax = 30)

  # Only the prediction (not the variance), back to WE units as above
  krig_we <- predict(krig_mod, my_we_grid_biome)
  my_we_grid_biome$krig.we <- exp(krig_we$we.pred)

  # Richness of the biome endemics (S_obs of the same cells), same way as WE
  v_rich <- automap::autofitVariogram(log(S_obs) ~ 1, my_we_pts_sp)
  krig_mod_rich <- gstat::gstat(id = "S_obs",
                                formula = log(S_obs) ~ 1,
                                locations = ~xm + ym,
                                data = as.data.frame(my_we_pts_sp),
                                model = v_rich$var_model,
                                nmax = 30)
  my_we_grid_biome$krig.s.obs <- exp(predict(krig_mod_rich, my_we_grid_biome)$S_obs.pred)
  x_final <- terra::rast(my_we_grid_biome[, c("x", "y", "krig.we", "krig.s.obs")],
                         type = "xyz", crs = "epsg:4326")
  x_final <- terra::mask(terra::crop(x_final, biome_poly), biome_poly)

  my_we_grid_biomes[[i]] <- x_final
}

# Save separately
filenames <- paste0("interpolated_maps_biomes_50km_",
                    make.names(names(my_we_grid_biomes)), ".tiff")

for (i in seq_along(my_we_grid_biomes)) {
  terra::writeRaster(my_we_grid_biomes[[i]], 
                     here::here("data", "derived-data", "maps", filenames[i]),
                     overwrite = TRUE)
}
