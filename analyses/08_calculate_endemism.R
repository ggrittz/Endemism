
# Calculate endemism ------------------------------------------------------
devtools::load_all()


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

  # Keeping all columns. They were filtered before anyway
  all_fam[[i]] <- readRDS(family_path) # [, c("decimalLongitude.new", "decimalLatitude.new",
                                       # "scientificName", "family", "our_efg", "is_nt", "efg_endemic",
                                       # "loc.correct", "tax.check", "identifiedBy.new")]
}

# Bind it all
all_fam <- dplyr::bind_rows(all_fam)

##### Before calculating the metrics, there are a few minor manipulations needed #####
# Keep only records validated by a taxonomists ($tax.check == 'high')
# high_names <- unique(all_fam$scientificName[all_fam$tax.check %in% "high"])
all_fam1 <- all_fam[all_fam$tax.check %in% "high", ]

# Get only "trees" lifeForm
##### Should I add the source here too? #####
all_fam1 <- dplyr::left_join(all_fam1, our_names1[, c("our_accepted_scientificName", 
                                                     "lifeForm_aggregated")],
                             by = c("scientificName" = "our_accepted_scientificName"))
all_fam1 <- all_fam1[grepl("tree", all_fam1$lifeForm_aggregated), ]

# Dealing with transition zones and predominance --------------------------
# How many registers have 'Transition' or 'predominant' in its name?
table(grepl("\\|", all_fam1$our_efg) |
        grepl("predominant", all_fam1$our_efg)) # 28259 / 1123931 (2%)

##### How many families, genus, and species are endemic? #####
length(unique(all_fam1$family)) # 206 families
length(unique(sapply(strsplit(all_fam1$scientificName, " "), 
                     function(x) x[1]))) # 1,954 genus
length(unique(all_fam1$scientificName)) # 24,693 species/subspecies/varieties

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

# Get efg area
result <- dplyr::left_join(result, 
                           terra::as.data.frame(efg[, c("our_efg", "er_Area_km")], geom = NULL),
                           by = c("EFG" = "our_efg"))
names(result)[which(names(result) == "er_Area_km")] <- "EFG_area_km"

# if(!dir.exists(here::here("outputs", "tables")))
#   dir.create(here::here("outputs", "tables"))
# write.csv(result, here::here("outputs", "tables", "efg_summary_table.csv"))

# Generate species lists per EFG
species_list <- lapply(split(expanded_df, expanded_df$our_efg), 
                       function(x) unique(x$scientificName))

# Generate endemics species lists per EFG
species_list_end <- lapply(split(expanded_df, expanded_df$efg_endemic), 
                           function(x) unique(x$scientificName))

# Save these
# path_to_save <- here::here("data", "derived-data", "rds")
# file1 <- "spp_per_efg.rds"
# file2 <- "end_per_efg.rds"
# saveRDS(species_list, file.path(path_to_save, file1))
# saveRDS(species_list_end, file.path(path_to_save, file2))


# What names are missing from our registers? (only good taxonomic sources here)
##### Or keep all sources anyway? #####
all_trees <- our_names1$our_accepted_scientificName[
  grepl("tree",our_names1$lifeForm_aggregated) & 
    grepl("FBO|GTS|VPA|WCVP", our_names1$source)]

##### To do this properly, we may need another filter...these are Americas' names, not #####
##### only tropical America... #####
# These registers occur in the Americas but we didn't found a good taxonomist
# to vouch for them
missing_trees <- setdiff(all_trees, unique(all_fam1$scientificName))
# saveRDS(missing_trees, file.path("data", "derived-data", "rds", "missing_trees.rds"))




##### Finally, calculating weighted endemism #####

# Weighted Endemism -------------------------------------------------------

# Grid cell resolution to be used
base_res <- 0.5 # 111.32 km = 1 grid degree at Equator

# Create template raster
r_template <- terra::rast(extent = terra::ext(efg),
                          resolution = base_res,
                          crs = "epsg:4326")

# Rasterize the Tropical America map into the template
raster_output <- terra::rasterize(efg, r_template, field = "our_efg")
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
all_fam2 <- all_fam1[all_fam1$is_nt %in% TRUE, ] # 1,142,761 registers
# saveRDS(all_fam2, file.path("data", "derived-data", "rds", "final_df.rds"))


# Save all_fam2 df in the NDM/VNDM format for posterior analyses

##### Prepare final df for endemicity analysis in VNDM #####
a <- final_df[, c("scientificName", "decimalLatitude.new", "decimalLongitude.new")]
b <- a[grepl("Ocotea porosa|Araucaria", a$Species), ]
names(a) <- c("Species", "Latitude", "Longitude")
path_to_save <- here::here("data", "derived-data", "occ_forNVDM.csv")
path_to_save1 <- here::here("data", "derived-data", "teste.csv")

readr::write_delim(b, path_to_save1, delim = ";")












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
# Endemism null test
we_map_area_null <- endemism.null.test(we_map_area,
                                       nrep = 100,
                                       outlier.range = 1.5)

# Store both outputs for later use, if needed
# path_to_save <- here::here("data", "derived-data", "rds")
# name_to_save0 <- "we_map_area_50km_null.rds"
# name_to_save1 <- "we_map_area_50km.tiff"
# # if(!dir.exists(path_to_save))
#   dir.create(path_to_save)
#
# terra::writeRaster(we_map_area$WE_raster, file.path(path_to_save, name_to_save1))
# saveRDS(we_map_area_null, file.path(path_to_save, name_to_save0))


# Load only necessary objects for analyses --------------------------------

# we_map_span <- readRDS(here::here("data", "derived-data", "rds",
#                                  "we_map_span.rds"))

final_df <- readRDS(here::here("data", "derived-data", "rds",
                               "final_df.rds"))
we_map_area <- readRDS(here::here("data", "derived-data", "rds",
                                 "we_map_area_50km.rds"))
we_map_area_null <- readRDS(here::here("data", "derived-data", "rds",
                                       "we_map_area_50km_null_model.rds"))
# Study area
tropical_america <- here::here("data", "derived-data", "maps", "NTAmericas_dissolved.shp")
tropical_america <- terra::vect(tropical_america, crs = "epsg:4326")
tropical_america <- tropical_america[tropical_america$Morrone_re %in% "Neotropical", ]


# Dealing with sampling coverage ------------------------------------------
# Convert the {raster} map to {terra}
my_we_raster <- terra::rast(we_map_area$WE_raster, crs = "epsg:4326")
terra::values(my_we_raster) <- raster::values(we_map_area$WE_raster)
# Convert occ points to spatial points
final_df1 <- terra::vect(final_df, 
                         geom = c("decimalLongitude.new",
                                  "decimalLatitude.new"), 
                         crs = "epsg:4326")

# Get cell numbers for all points
cell_numbers <- terra::cellFromXY(my_we_raster, terra::crds(final_df1))

# Create a list split by cell numbers
cell_species <- split(final_df1$scientificName, cell_numbers)

# Count abundances per species per cell
abundance_list <- lapply(cell_species, function(x) {
  table(x)
  })
abundance_list <- abundance_list[!is.na(names(abundance_list))]

# Only the vector of values
abundance_vectors <- lapply(abundance_list, as.vector)

# Measure the sampling coverage (considering N/cell as abundance data type)
sc_df = iNEXT::DataInfo(abundance_vectors, datatype = "abundance")

# Asymptotic estimates for diversity metrics
library(parallel)
library(doParallel)

cl <- makeCluster(detectCores()-4)
clusterEvalQ(cl, library("iNEXT"))
clusterExport(cl, list("abundance_vectors"), envir = environment())

tmp = parLapply(cl, 1:length(abundance_vectors), fun = function(x) {
  current_abundance <- abundance_vectors[[x]]
  cell_name <- names(abundance_vectors)[x]
  
  tmp1 = iNEXT(current_abundance, datatype = "abundance", q = 0, 
               size = 100, endpoint = 100, knots = 2, se = FALSE)$AsyEst
  
  tmp1 <- data.frame(cell_id = cell_name,
                     Diversity = rownames(tmp1),
                     tmp1[, 1:3],
                     row.names = NULL)
  
  return(tmp1)
  
})

stopCluster(cl)

# Convert results to data frame
diversity_df = do.call("rbind.data.frame", tmp)
diversity_df <- diversity_df[grepl("Richness", diversity_df$Diversity), ]
diversity_df$cell_id <- as.numeric(diversity_df$cell_id)
names(diversity_df)[which(names(diversity_df) == "Estimator")] <- "S.est"

##### Get this information back to the raster #####
cell_id <- as.numeric(names(abundance_vectors))
sc_df <- data.frame(cell_id = as.numeric(sc_df$Assemblage),
                    n = sc_df$n,
                    S.obs = sc_df$S.obs,
                    SC = sc_df$SC)

# Get coordinates for these specific cells
cell_coords <- data.frame(cell_id = cell_id,
                          x = terra::xFromCell(my_we_raster, cell_id),
                          y = terra::yFromCell(my_we_raster, cell_id))
# Now the metrics
cell_metrics <- dplyr::left_join(sc_df,
                                 cell_coords,
                                 by = "cell_id")
cell_metrics <- dplyr::left_join(cell_metrics, diversity_df[, c(1, 4)],
                                 by = "cell_id")

# Populate new layers
my_we_raster <- c(my_we_raster,
                  terra::rast(my_we_raster, nlyrs = 4))
names(my_we_raster)[(terra::nlyr(my_we_raster)-3):terra::nlyr(my_we_raster)] <- 
  c("sample_coverage", "n", "S.obs", "S.est")

my_we_raster[["sample_coverage"]][cell_metrics$cell_id] <- cell_metrics$SC
my_we_raster[["n"]][cell_metrics$cell_id] <- cell_metrics$n
my_we_raster[["S.obs"]][cell_metrics$cell_id] <- cell_metrics$S.obs
my_we_raster[["S.est"]][cell_metrics$cell_id] <- cell_metrics$S.est

# Which cells are going to be used for interpolation?
my_we_raster1 <- terra::as.data.frame(my_we_raster, xy = TRUE, cells = TRUE)

##### Using sample coverage and N cut-offs following Lima et al. 2020 #####

# Get the quantiles for SC
sc_quantiles <- quantile(my_we_raster1$sample_coverage, probs = c(0.25, 0.50, 0.75))

# Condition 1: between 25 and 50 quantiles of sc and more than 100 occ.
cond1 <- my_we_raster1$sample_coverage >= sc_quantiles["25%"] & 
  my_we_raster1$sample_coverage <= sc_quantiles["50%"] & 
  my_we_raster1$n >= 75

# Condition 2: between 50 and 75 quantiles of sc and more than 50 occ.
cond2 <- my_we_raster1$sample_coverage >= sc_quantiles["50%"] & 
  my_we_raster1$sample_coverage <= sc_quantiles["75%"] & 
  my_we_raster1$n >= 35

# Condition 3: more than 75 quantiles of sc and more than 30 occ.
cond3 <- my_we_raster1$sample_coverage >= sc_quantiles["75%"] & 
  my_we_raster1$n >= 30

# Tag rows that meet any condition
my_we_raster1$use_this <- cond1 | cond2 | cond3
# Changing one name
names(my_we_raster1)[which(names(my_we_raster1) == "our_efg")] <- "we"



# Interpolating diversity metrics -----------------------------------------

# Grid to predict the the metrics
my_we_grid <- terra::rast(extent = terra::ext(my_we_raster),
                          resolution = 0.25,
                          crs = terra::crs(my_we_raster))
my_we_grid <- terra::as.data.frame(my_we_grid, xy = TRUE, na.rm = FALSE)
my_we_grid$lyr.1 <- NULL

# Using only the minimum SC points
my_we_pts_sp <- my_we_raster1[my_we_raster1$use_this %in% TRUE, ]
sp::coordinates(my_we_pts_sp) <- ~x+y

##### Interpolating weighted endemism #####

# Make variogram
v <- automap::autofitVariogram(log(we) ~ 1, my_we_pts_sp)
vario <- v$var_model
spp_corr_we <- vario$range[2]
krig_mod <- gstat::gstat(id = "we",
                         formula = log(we) ~ 1, # Interpolating log-transformed values
                         locations = ~x+y,
                         data = my_we_raster1[my_we_raster1$use_this %in% TRUE, ],
                         model = gstat::vgm(model = "Ste", # find a better way to write this down
                                            psill = vario[2, 2],
                                            range = vario[2, 3],
                                            kappa = vario[2, 4]),
                         nmax = 30)



# Setting the USSE method as the minimum sample coverage value in our data
# my_we_grid$n <- (my_we_raster1$n[my_we_raster1$use_this %in% TRUE])
# my_we_grid$SC <- min(my_we_raster1$SC[my_we_raster1$use_this %in% TRUE])

# Predict the WE model in the grid
my_we_grid$krig.we <- predict(krig_mod, my_we_grid)$we.pred


##### Interpolating observed species richness #####

# Make variogram
v <- automap::autofitVariogram(log(S.obs) ~ 1, my_we_pts_sp)
vario <- v$var_model
spp_corr_sr <- vario$range[2]
krig_mod <- gstat::gstat(id = "S.obs",
                         formula = log(S.obs) ~ 1, # Interpolating log-transformed values
                         locations = ~x+y,
                         data = my_we_raster1[my_we_raster1$use_this %in% TRUE, ],
                         model = gstat::vgm(model = "Ste", # find a better way to write this down
                                            psill = vario[2, 2],
                                            range = vario[2, 3],
                                            kappa = vario[2, 4]),
                         nmax = 30)

# Predict the observed species richness model in the grid
my_we_grid$krig.s.obs <- predict(krig_mod, my_we_grid)$S.obs.pred


##### Interpolating asymptotic species richness q = 0 #####

# Make variogram
v <- automap::autofitVariogram(log(S.est) ~ 1, my_we_pts_sp)
vario <- v$var_model
spp_corr_sre <- vario$range[2]
krig_mod <- gstat::gstat(id = "S.est",
                         formula = log(S.est) ~ 1, # Interpolating log-transformed values
                         locations = ~x+y,
                         data = my_we_raster1[my_we_raster1$use_this %in% TRUE, ],
                         model = gstat::vgm(model = "Ste", # find a better way to write this down
                                            psill = vario[2, 2],
                                            range = vario[2, 3],
                                            kappa = vario[2, 4]),
                         nmax = 30)

# Predict the observed species richness model in the grid
my_we_grid$krig.s.est <- predict(krig_mod, my_we_grid)$S.est.pred


# Convert to raster for cropping, masking, and plotting
my_we_grid <- terra::rast(x = my_we_grid, crs = "epsg:4326")

# Crop and mask
my_int <- terra::crop(my_we_grid, tropical_america)
my_int <- terra::mask(my_int, tropical_america)
# terra::plot(my_int$krig.fit, col = terrain.colors(100, rev = TRUE))

th_krig_we <- terra::global(my_int$krig.s.obs, fun = quantile, probs = 0.9, na.rm = TRUE)[1,1]
terra::plot(my_int$krig.we, 
            #xlab = "Longitude",
            #ylab = "Latitude",
            main = "Weighted endemism",
            cex.lab = 1.5,
            pax = list(cex.axis = 1.4),
            col = terrain.colors(100, rev = TRUE), 
            legend = F)
terra::contour(my_int$krig.we, add = TRUE, levels = th_krig_we, col = "black", lwd = 3,
               drawlabels = FALSE)
terra::plot(my_int$krig.s.obs > th_krig_we, main = "Estimated richness", legend = F)


# Save this result
terra::writeRaster(my_int, here::here("data", "derived-data", 
                                      "maps", "interpolated_maps25km.tiff"),
                   overwrite = TRUE)



# Some metrics and plots --------------------------------------------------

##### GLS is the best estimator #####

# Subsetting the data to run gls (~ 1,000 points randomly choosen)
##### Maybe 2,000 points is ok...test later #####

# This is needed to properly compare the metrics
my_we_raster_true <- my_we_raster1[my_we_raster1$use_this %in% TRUE, ]
sample_size <- round(0.5 * nrow(my_we_raster_true))
sample_indices <- sample(1:nrow(my_we_raster_true), 
                         size = sample_size)
my_we_raster_true <- my_we_raster_true[sample_indices, ]

# For weighted endemism
tmp0 = lm(log(we) ~ 1, data = my_we_raster_true)
tmp1  = MASS::rlm(log(we) ~ log(n), data = my_we_raster_true)
tmp2 = lm(log(we) ~ log(n), data = my_we_raster_true, weights = my_we_raster_true$sample_coverage)
tmp3 = lm(log(we) ~ log(n), data = my_we_raster_true, weights = my_we_raster_true$sample_coverage^3)
tmp4 = nlme::gls(log(we) ~ log(n), data = my_we_raster_true, 
                 corr = nlme::corExp(spp_corr_we, form= ~x+y, nugget = FALSE), 
                 control = list(maxIter = 20, tolerance = 0.01))

# For observed richness
tmp00 = lm(log(S.obs) ~ 1, data = my_we_raster_true)
tmp11  = MASS::rlm(log(S.obs) ~ log(n), data = my_we_raster_true)
tmp22 = lm(log(S.obs) ~ log(n), data = my_we_raster_true, weights = my_we_raster_true$sample_coverage)
tmp33 = lm(log(S.obs) ~ log(n), data = my_we_raster_true, weights = my_we_raster_true$sample_coverage^3)
tmp44 = nlme::gls(log(S.obs) ~ log(n), data = my_we_raster_true,
                  corr = nlme::corExp(spp_corr_we, form= ~x+y, nugget = FALSE),
                  control = list(maxIter = 20, tolerance = 0.01))

# Model selection using AICc
mod_comparions <- performance::compare_performance(tmp00, tmp11, tmp22, tmp33, tmp44)
(best_model <- mod_comparions$Model[which.min(mod_comparions$AIC)])


jpeg(filename = "FigureS3.jpg", width = 3100*1.5, height = 3000*1.5, units = "px", 
     pointsize = 12, res = 600, family = "sans", type="cairo", bg="white")
par(mfrow = c(2,2)) #, las=1,mar=c(3,3,0.5,0.25),mgp=c(1.8,0.35,0),tcl=-.25)
plot(log(we) ~ log(n), 
     data = my_we_raster_true,
     # pch = 19,
     xlab = "Number of occurrences", 
     ylab = "Weighted endemism")
curve(predict(tmp4, newdata = data.frame(n = exp(x))), 
      col = "red",
      lwd = 2, 
      add = TRUE)

R2 <- cor(log(my_we_raster_true$we),predict(tmp4))^2
df = tmp4$dims$N - tmp4$dims$p
legend("bottomright", legend = do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp4)$F[2], 1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty = "n", horiz = F, cex = 1, adj = c(0, 0), inset = c(-0.01,-0.03)
)

plot(log(S.obs) ~ log(n), 
     data = my_we_raster_true,
     # pch = 19,
     xlab = "Number of occurrences", 
     ylab = "Weighted endemism")
curve(predict(tmp44, newdata = data.frame(n = exp(x))), 
      col = "red",
      lwd = 2, 
      add = TRUE)

R2 <- cor(log(my_we_raster_true$S.obs),predict(tmp44))^2
df = tmp44$dims$N - tmp4$dims$p
legend("bottomright", legend = do.call(expression, list(
  bquote(italic(F)~"- test="~.(round(anova(tmp44)$F[2], 1))),
  bquote("d.f. = "~.(df)),
  bquote({pseudo-italic(R)^2}~"="~.(round(R2*100,1))~"%")
)),
bty = "n", horiz = F, cex = 1, adj = c(0, 0), inset = c(-0.01,-0.03)
)








