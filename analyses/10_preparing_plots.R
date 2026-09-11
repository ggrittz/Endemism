
# Plots and figures and stuff ---------------------------------------------

# Load necessary data
file_path0 <- here::here("data", "derived-data")
file_path1 <- here::here("data", "derived-data", "rds")
file_path2 <- here::here("data", "derived-data", "maps")
file_path3 <- here::here("outputs", "tables")
file_path4 <- here::here("data", 'raw-data', "maps")
file1 <- "end_per_efg.rds"
file2 <- "spp_per_efg.rds"
file3 <- "we_map_area_50km.tiff"
# Substitute here later for 1 km map
file4 <- "interpolated_maps1km.tiff"
file44 <- "_biomes_"
file5 <- "final_df.rds"
file6 <- "missing_trees.rds"
file7 <- "checklist_standardized.rds"
# file8 <- "we_map_area_50km_null_model.rds"
file9 <- "efg_summary_table.xlsx"
file10 <- "hotspots_2016_1.shp"

end_efg <- readRDS(file.path(file_path1, file1))
spp_efg <- readRDS(file.path(file_path1, file2))
wee_map <- terra::rast(file.path(file_path2, file3)) 
int_map <- terra::rast(file.path(file_path2, file4))
# biomes...
all_files <- list.files(file_path2, full.names = TRUE)
biome_files <- all_files[grepl("_biomes_", all_files)]
# biome_files <- biome_files[2:6]
biome_names <- gsub("^.*_([^_]+)\\.tiff$", "\\1", basename(biome_files))
# biome_names <- biome_names[2:6]
int_map_biomes <- vector("list", length(biome_files))
for (i in seq_along(biome_files)) {
  int_map_biomes[[i]] <- terra::rast(biome_files[i])
}
names(int_map_biomes) <- biome_names

# null_model <- readRDS(file.path(file_path1, file8))
# scatt <- read.csv(file.path(file_path3, file9))
my_df <- readRDS(file.path(file_path1, file5)) 
my_df1 <- my_df[, c("decimalLongitude.new", "decimalLatitude.new")]
my_df1$decimalLongitude.new <- as.numeric(my_df1$decimalLongitude.new)
my_df1$decimalLatitude.new <- as.numeric(my_df1$decimalLatitude.new)
my_df1 <- terra::vect(my_df1, 
                      geom = c("decimalLongitude.new",
                               "decimalLatitude.new"),
                      crs = "epsg:4326")
# our_names <- readRDS(file.path(file_path0, file7))
# a <- our_names[grepl("herb", our_names$lifeForm_aggregated),]

# Shapefiles...
america <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp"))
# Separate tropical America
tropical_america <- america[america$Morrone_re %in% "Neotropical", ]
# tropical_america$our_efg <- as.character(tropical_america$our_efg)
# Adding the GET Codes to the legend and also changing the name of one EFG
# From Tropical alpine grasslands and shrublands
# To Tropical alpine grasslands and herbfields
tropical_america$our_efg <- gsub("Tropical alpine grasslands and shrublands",
                                 "Tropical alpine grasslands and herbfields",
                                 tropical_america$our_efg)

##### THERE IS A NEW NAME TO CHANGE CHECK IUCN TYPOLOGY #####


# GET codes from IUCN
getCodes <- c("T5.4", "T5.5", "MFT1.2", "T2.3", "T4.2", "TF1.4", "T3.2",
              "T3.1", "T6.4", "T4.5", "T4.4", "T5.2", "T6.5", "TF1.1",
              "T1.4", "T1.2", "T1.1", "T1.3", "T2.4")
# Add the codes to the names
tropical_america$our_efg <- paste0(tropical_america$our_efg, " ", "(", getCodes, ")")

tropical_america_diss <- terra::aggregate(tropical_america, cores = 8)
tropical_america_diss <- terra::fillHoles(tropical_america_diss)
# Hotspots nt 2016
hotspots <- terra::vect(file.path(file_path4, file10))
# Only hotspot area, not outer limits
hotspots <- hotspots[hotspots$Type %in% "hotspot area", ]
hotspots <- terra::crop(hotspots, tropical_america_diss)
hotspots <- terra::mask(hotspots, tropical_america_diss)

# WDPA for tropical America
# Considering only classes I and II from IUCN and only terrestrial PAs
# options(timeout = 10000)
# getOption('timeout')
# download.file(url = "https://d1gam3xoknrgr2.cloudfront.net/current/WDPA_Feb2026_Public_shp.zip",
#               destfile = here::here("data", "raw-data", "WDPA_Feb_2026.zip"))
# zip_name <- here::here("data", "raw-data", "WDPA_Feb_2026.zip")
# unzip(zipfile = zip_name, exdir = here::here("data", "raw-data", "WDPA_Feb_2026"))
# Nested zipfiles...
# zip_files <- list.files(path = here::here("data", "raw-data", "WDPA_Feb_2026"),
#                         pattern = ".zip",
#                         full.names = TRUE)
# 
# for (i in seq_along(zip_files)) {
#   folder_name <- paste0("WDPA_Feb_2026_shapefiles", "_", i - 1)
#   output_path <- file.path(here::here("data", "raw-data"), folder_name)
#   if (!dir.exists(output_path)) {
#     dir.create(output_path, recursive = TRUE)
#   }
#   unzip(zip_files[i], exdir = output_path)
# }

# Load the polygons, prepare and save to derived-data/maps
# zip_folders <- list.files(here::here("data", "raw-data"), 
#                           pattern = "_shapefiles_",
#                           full.names = TRUE)
# 
# all_polygons <- list()
# 
# for(i in seq_along(zip_folders)){
#   wdpa_polygons <- sf::st_read(file.path(zip_folders[i], 
#                                          "WDPA_Feb2026_Public_shp-polygons.shp"))
#   
#   # Only IUCN_CAT Ia, Ib, and II
#   wdpa_polygons <- wdpa_polygons[wdpa_polygons$IUCN_CAT %in% c("Ia", "Ib", "II", "III"), ]
#   # Only terrestrial PAs
#   wdpa_polygons <- wdpa_polygons[wdpa_polygons$REALM %in% "Terrestrial", ]
#   # Only designated or already established
#   wdpa_polygons <- wdpa_polygons[wdpa_polygons$STATUS %in% c("Designated", "Established"), ]
#   # Convert to terra and save
#   wdpa_polygons_terra <- terra::vect(wdpa_polygons)
#   # Crop and mask the PAs into tropical America
#   wdpa_polygons_terra_final <- terra::crop(wdpa_polygons_terra, terra::ext(tropical_america_diss))
#   wdpa_polygons_terra_final <- terra::buffer(wdpa_polygons_terra_final, 0)
#   wdpa_polygons_terra_final <- terra::intersect(wdpa_polygons_terra_final, 
#                                                 tropical_america_diss)
#   
#   # Save
# all_polygons[[i]] <- wdpa_polygons_terra_final
#     
# }
# 
# all_polygons1 <- terra::vect(all_polygons)
# # Save this in derived-data
# pa_path <- here::here("data", "derived-data", "maps")
# pa_name <- "tropicalAmerica_WDPA_Feb2026_cleaned.shp"
# terra::writeVector(all_polygons1, file.path(pa_path, pa_name), overwrite = TRUE)

# Whole
america <- terra::aggregate(america, cores = 8)
america <- terra::fillHoles(america)

# Preparing the study region map ------------------------------------------
cores <- MetroBrewer::metro.brewer("Beijing", 
                                   n = 19,
                                   return_hex = T)

names(cores) <- tropical_america$our_efg

# Add specific colors for the same biomes in STS paper 1
cores[which(grepl("lowland", names(cores)))] <- "#098137"
cores[which(grepl("thickets", names(cores)))] <- "#fdcf07"
cores[which(grepl("savanna", names(cores)))] <- "#ed2621"
cores[which(grepl("montane", names(cores)))] <- "#94989b"
cores[which(grepl("woodlands", names(cores)))] <- "#2d9ee0"
cores[which(grepl("flooded", names(cores)))] <- "#253d95"
cores[which(grepl("laurophyll", names(cores)))] <- "#960153"
cores[which(grepl("heath", names(cores)))] <- "#f368a1"
cores[which(grepl("marshes", names(cores)))] <- "#964B00"
cores[which(grepl("tropical shrublands$", names(cores)))] <- "#D1FFBD"
cores[which(grepl("^Hyper", names(cores)))] <- "#CCAD60"
cores[which(grepl("^Cool", names(cores)))] <- "#FF7F7F"

tropical_america$our_efg <- factor(tropical_america$our_efg, levels = names(cores))

svg("figures/Figure1.svg", width = 10, height = 10)
# Main plot of Neotropics
terra::plot(tropical_america, 
            col = cores[tropical_america$our_efg],
            # main = "Neotropical Ecossystem Functional Groups",
            xlab = "Longitude",
            ylab = "Latitude",
            cex.lab = 1.5,
            pax = list(cex.axis = 1.4))

# The rest of it
terra::plot(america,
            add = TRUE)
#xlab = "Longitude",
#ylab = "Latitude",
#cex.lab = 1.5,
#pax = list(cex.axis = 1.4))

# Scale
terra::sbar(d = 1000, xy = "bottomright", type = "bar", div = 3,
            lonlat = TRUE, below = "km")
# Legend
legend(x = -115,
       y = -5,
       legend = names(cores),
       fill = cores,
       y.intersp = 1,
       x.intersp = 1,
       cex = 1,
       ncol = 1,
       bty = "n")
dev.off()


# Calculate density map for observations ----------------------------------
dens <- terra::rasterize(x = my_df1, y = wee_map, fun = "sum")
##### Fix this on the code in the next version! #####
##### Aka, do mask off these registers from the final_df #####
dens <- terra::mask(dens, tropical_america_diss)
quantile_vals <- terra::global(dens, quantile, probs = seq(0, 1, 0.05), na.rm = TRUE)
breaks <- as.numeric(quantile_vals)
labels <- names(quantile_vals)

##### 90th percent quantile for all metrics #####

# Observed weighted endemism
th_we <- terra::global(int_map$krig.we, fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
th_we_bin <- int_map$krig.we > th_we
th_we_bin <- terra::as.polygons(th_we_bin, dissolve = TRUE)
# Keep only 1
th_we_bin <- th_we_bin[th_we_bin$krig.we == 1, ]
# Disaggregate to measure polygon sizes
th_we_bin_dis <- terra::disagg(th_we_bin)
th_we_bin_area <- terra::expanse(th_we_bin_dis, unit = "km")
th_we_bin_dis$area_km2 <- round(th_we_bin_area, 1)
# Keeping only > 50 km² polygons to ease big pattern visualization and also since
# this is our base area
# th_we_bin_dis <- th_we_bin_dis[th_we_bin_dis$area_km2 > 50, ]
# terra::plot(int_map$krig.we, main = "xxx", col = hcl.colors(n = 100, "RdYlBu", rev = T))
# terra::plot(th_we_bin_dis, lwd = 3, add = T)

# Observed richness
th_s.obs <- terra::global(int_map$krig.s.obs, fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
th_s.obs_bin <- int_map$krig.s.obs > th_s.obs
th_s.obs_bin <- terra::as.polygons(th_s.obs_bin, dissolve = TRUE)
# Keep only 1
th_s.obs_bin <- th_s.obs_bin[th_s.obs_bin$krig.s.obs == 1, ]
# Disaggregate to measure polygon sizes
th_s.obs_bin_dis <- terra::disagg(th_s.obs_bin)
th_s.obs_bin_area <- terra::expanse(th_s.obs_bin_dis, unit = "km")
th_s.obs_bin_dis$area_km2 <- round(th_s.obs_bin_area, 1)
# Keeping only > 50 km² polygons to ease big pattern visualization and also since
# this is our base area
th_s.obs_bin_dis <- th_s.obs_bin_dis[th_s.obs_bin_dis$area_km2 > 50, ]
# terra::plot(int_map$krig.s.obs, main = "xxx", col = hcl.colors(n = 100, "RdYlBu", rev = T))
# terra::plot(th_s.obs_bin_dis, lwd = 3, add = T)

# Observed Simpson
th_simp.obs <- terra::global(int_map$krig.simp.obs, fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
th_simp.obs_bin <- int_map$krig.simp.obs > th_simp.obs
th_simp.obs_bin <- terra::as.polygons(th_simp.obs_bin, dissolve = TRUE)
# Keep only 1
th_simp.obs_bin <- th_simp.obs_bin[th_simp.obs_bin$krig.simp.obs == 1, ]
# Disaggregate to measure polygon sizes
th_simp.obs_bin_dis <- terra::disagg(th_simp.obs_bin)
th_simp.obs_bin_area <- terra::expanse(th_simp.obs_bin_dis, unit = "km")
th_simp.obs_bin_dis$area_km2 <- round(th_simp.obs_bin_area, 1)
# Keeping only > 50 km² polygons to ease big pattern visualization and also since
# this is our base area
th_simp.obs_bin_dis <- th_simp.obs_bin_dis[th_simp.obs_bin_dis$area_km2 > 50, ]
# terra::plot(int_map$krig.simp.obs, main = "xxx", col = hcl.colors(n = 100, "RdYlBu", rev = T))
# terra::plot(th_simp.obs_bin_dis, lwd = 3, add = T)

# Make layout
# my_lay <- matrix(c(1, 2, 3, 7, 1, 4, 5, 7, 6, 6, 6, 6),
#                  nrow = 3, ncol = 4, byrow = TRUE)




# New figures: Figure 2, Figure S1, Figure 3? -----------------------------

##### Figure 2 #####
{
svg("figures/Figure2.svg", width = 10, height = 10)

terra::plot(int_map$krig.we, 
            col = hcl.colors(n = 100, "RdYlBu", rev = T),
            main = "Weighted endemism",
            cex.main = 1.5,
            xlab = "Longitude",
            ylab = "Latitude",
            cex.lab = 1.5,
            pax = list(cex.axis = 1.4))

# Scale
terra::sbar(d = 1000, xy = "bottomright", type = "bar", div = 3,
            lonlat = TRUE, below = "km")

# Only above 50 km² to ease visualization
terra::plot(th_we_bin_dis[th_we_bin_dis$area_km2 > 50, ], lwd = 3, add = T)
terra::plot(america, add = TRUE)

dev.off()
}

##### Figure S2 #####
{
  svg("figures/FigureS2.svg", width = 10, height = 10)
  
  terra::plot(int_map$krig.s.obs, 
              col = hcl.colors(n = 100, "RdYlBu", rev = T),
              main = "Species richness",
              cex.main = 1.5,
              xlab = "Longitude",
              ylab = "Latitude",
              cex.lab = 1.5,
              pax = list(cex.axis = 1.4))
  
  # Scale
  terra::sbar(d = 1000, xy = "bottomright", type = "bar", div = 3,
              lonlat = TRUE, below = "km")
  
  # Only above 50 km² to ease visualization
  # terra::plot(th_we_bin_dis[th_we_bin_dis$area_km2 > 50, ], lwd = 3, add = T)
  terra::plot(america, add = TRUE)
  
  dev.off()
}

##### Figure S3 #####
{
  svg("figures/FigureS3.svg", width = 10, height = 10)
  
  terra::plot(log(dens), 
              col = hcl.colors(n = 100, "RdYlBu", rev = T),
              main = "Sampling effort (50 × 50 km²)",
              cex.main = 1.5,
              xlab = "Longitude",
              ylab = "Latitude",
              cex.lab = 1.5,
              pax = list(cex.axis = 1.4))
  
  # Scale
  terra::sbar(d = 1000, xy = "bottomright", type = "bar", div = 3,
              lonlat = TRUE, below = "km")
  
  # Only above 50 km² to ease visualization
  # terra::plot(th_we_bin_dis[th_we_bin_dis$area_km2 > 50, ], lwd = 3, add = T)
  terra::plot(america, add = TRUE)
  
  dev.off()
}

##### Old figure code #####

# my_lay <- matrix(c(1, 2, 3, 5, 1, 2, 3, 5, 4, 4, 4, 4),
#                  nrow = 3, ncol = 4, byrow = TRUE)
# 
# {
# svg("figures/Figure22.svg", width = 32.5, height = 16)
# 
# layout(my_lay, 
#        widths = c(0.05, 0.4375, 0.4375, 0.05),
#        heights = c(0.475, 0.475, 0.045))
# layout.show(5)
# 
# # Shared y-axis name
# par(mar = c(0, 0, 0, 0))
# plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
# mtext("Latitude", side = 4, line = -8, cex = 4.5)
# 
# # Sampling effort
# par(mar = c(0, 0, 0, 0))
# terra::plot(log(dens),
#             main = "(a) Sampling effort",
#             col = hcl.colors(n = 100, "RdYlBu", rev = T),
#             cex.main = 6,
#             pax = list(cex.axis = 5, las = 1, mgp = c(3, 2.5, 0)),
#             legend = F,
#             axes = T)
# terra::plot(america, add = TRUE)
# 
# # # Simpson diversity
# # par(mar = c(0, 0, 0, 0))
# # terra::plot(int_map$krig.simp.obs,
# #             main = "(c) Simpson diversity",
# #             cex.main = 6,
# #             pax = list(cex.axis = 5, las = 1, mgp = c(3, 2.5, 0)),
# #             col = hcl.colors(n = 100, "RdYlBu", rev = T),
# #             legend = F,
# #             axes = T)
# # terra::plot(th_simp.obs_bin_dis, lwd = 5, add = T)
# # terra::plot(america, add = TRUE)
# 
# # Weighted endemism
# par(mar = c(0, 0, 0, 0))
# terra::plot(int_map$krig.we, 
#             main = "(b) Weighted endemism",
#             cex.main = 6,
#             pax = list(cex.axis = 5, las = 1, yaxt = "n", mgp = c(3, 2.5, 0)),
#             col = hcl.colors(n = 100, "RdYlBu", rev = T), 
#             legend = F,
#             axes = T)
# # Only above 50 km² to ease visualization
# terra::plot(th_we_bin_dis[th_we_bin_dis$area_km2 > 50, ], lwd = 5, add = T)
# terra::plot(america, add = TRUE)
# # box()
# 
# # Shared x-axis
# par(mar = c(0, 0, 0, 0))
# plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
# mtext("Longitude", side = 3, line = -4, cex = 4.5)
# 
# # Legend bar
# par(mar = c(0, 0, 0, 5))
# plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")
# colramp <- hcl.colors(n = 100, "RdYlBu", rev = T)
# rect(0.2, 0:99, 0.8, 1:100, col = colramp, border = NA)
# axis(2, at = c(0, 100), labels = c("Low", "High"), las = 1, cex.axis = 4.5, line = -0.5)
# # text(0.5, -5, "Value", xpd = TRUE, cex = 1.2)
# dev.off()
# }
# 
# # # Disentangled values: how much did we separate the effects of sampling bias
# # # and richness from endemism?
# # # Mask on the density layer
# # rich_obs <- terra::resample(int_map$krig.s.obs, dens, method = "bilinear")
# # simp_obs <- terra::resample(int_map$krig.simp.obs, dens, method = "bilinear")
# # ende_obs <- terra::resample(int_map$krig.we, dens, method = "bilinear")
# # 
# # # Only not NA and NaN
# # # also back to the original value
# # df <- data.frame(rich = exp(terra::values(rich_obs)),
# #                  simp = exp(terra::values(simp_obs)),
# #                  ende = exp(terra::values(ende_obs)),
# #                  dens = terra::values(dens))
# # df <- na.omit(df)
# # cor(df$krig.s.obs, df$sum)
# # cor(df$krig.we, df$sum)
# # cor(df$krig.simp.obs, df$sum)


# Top 90% WE values inside each biome (area) ------------------------------
intersected <- terra::intersect(th_we_bin[th_we_bin$krig.we == 1, ], 
                                tropical_america)

# Area
intersected$top90_area_m2 <- terra::expanse(intersected)
intersected$top90_area_km2 <- intersected$top90_area_m2 / 1e6
# Aggregate the results by our_efg
result <- terra::aggregate(top90_area_km2 ~ our_efg, data = as.data.frame(intersected), FUN = sum)
intersected_df <- terra::as.data.frame(intersected)
intersected_df$prop_top90_area_km2 <- 100 * (intersected_df$top90_area_km2/intersected_df$er_Area_km)
keep_these <- c("our_efg", "Morrone_re", "er_Area_ha", "er_Area_km",
                "top90_area_m2", "top90_area_km2", "prop_top90_area_km2")
intersected_df <- intersected_df[, keep_these]
# Prepare and save this table
writexl::write_xlsx(intersected_df, file.path(file_path3, file9))


# Making one plot for each biome here -------------------------------------
{
  # Reduce outer margins to bring plots closer together
  # par(oma = c(0, 0, 0, 0))  # Remove outer margins completely
  # par(mar = c(0, 0, 0, 0))  # Keep inner margins at 0
svg("figures/Figure3.svg", width = 12, height = 10)

my_lay <- matrix(c(1, 2, 3, 4, 1, 5, 6, 4, 7, 7, 7, 7),
                  nrow = 3, ncol = 4, byrow = TRUE)
layout(my_lay, 
      widths = c(0.05, 0.4375, 0.4375, 0.05),
      heights = c(0.475, 0.475, 0.045))
layout.show(7)

# Shared y-axis name
par(mar = c(0, 0, 0, 0))
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
mtext("Latitude", side = 4, cex = 2)

# Plot biome per biome
my_order_biome_names <- biome_names

##### Biome 1 #####
biome1 <- my_order_biome_names[1]

th_we <- terra::global(int_map_biomes[[biome1]], fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
th_we_bin <- int_map_biomes[[biome1]] >= th_we
th_we_bin <- terra::as.polygons(th_we_bin, dissolve = TRUE)
th_we_bin <- th_we_bin[th_we_bin$lyr.1 == 1, ]

# par(mar = c(2, 3, 2, 2))
terra::plot(tropical_america_diss, pax = list(xat = FALSE, cex.axis = 2),
            main = paste("(a)", gsub("\\.", " ", biome1)), cex.main = 1.75)
terra::plot(int_map_biomes[[biome1]], 
            col = hcl.colors(n = 100, "RdYlBu", rev = T),
              # Use biome_name directly
            legend = FALSE,
            # xlab = "Longitude",
            # ylab = "Latitude",
            # cex.lab = 1.2,
            # pax = list(cex.axis = 1.1),
            add = TRUE)
terra::plot(th_we_bin, lwd = 2, add = TRUE)
terra::plot(america, add = TRUE)

##### Biome 2 #####
biome2 <- my_order_biome_names[2]

th_we <- terra::global(int_map_biomes[[biome2]], fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
th_we_bin <- int_map_biomes[[biome2]] >= th_we
th_we_bin <- terra::as.polygons(th_we_bin, dissolve = TRUE)
th_we_bin <- th_we_bin[th_we_bin$lyr.1 == 1, ]

par(mar = c(0, 0, 0, 0))
terra::plot(tropical_america_diss, pax = list(labels = FALSE, tick = FALSE),
            main = paste("(b)", gsub("\\.", " ", biome2)), cex.main = 1.75)
terra::plot(int_map_biomes[[biome2]], 
            col = hcl.colors(n = 100, "RdYlBu", rev = T),
              # Use biome_name directly
            legend = FALSE,
            # xlab = "Longitude",
            # ylab = "Latitude",
            # cex.lab = 1.2,
            # pax = list(cex.axis = 1.1),
            add = TRUE)
terra::plot(th_we_bin, lwd = 2, add = TRUE)
terra::plot(america, add = TRUE)

# Legend bar
par(mar = c(0, 0, 0, 0))
plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")
colramp <- hcl.colors(n = 100, "RdYlBu", rev = T)
rect(0.2, 0:99, 0.8, 1:100, col = colramp, border = NA)
axis(2, at = c(0, 100), labels = c("Low", "High"), las = 1, cex.axis = 1.75, line = -1)

##### Biome 3 #####
biome3 <- my_order_biome_names[3]

th_we <- terra::global(int_map_biomes[[biome3]], fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
th_we_bin <- int_map_biomes[[biome3]] >= th_we
th_we_bin <- terra::as.polygons(th_we_bin, dissolve = TRUE)
th_we_bin <- th_we_bin[th_we_bin$lyr.1 == 1, ]

par(mar = c(0, 0, 0, 0))
terra::plot(tropical_america_diss, pax = list(cex.axis = 2),
            main = paste("(c)", gsub("\\.", " ", biome3)), cex.main = 1.75)
terra::plot(int_map_biomes[[biome3]], 
            col = hcl.colors(n = 100, "RdYlBu", rev = T),
              # Use biome_name directly
            legend = FALSE,
            # xlab = "Longitude",
            # ylab = "Latitude",
            # cex.lab = 1.2,
            # pax = list(cex.axis = 1.1),
            add = TRUE)
terra::plot(th_we_bin, lwd = 2, add = TRUE)
terra::plot(america, add = TRUE)

##### Biome 4 #####
biome4 <- my_order_biome_names[4]

th_we <- terra::global(int_map_biomes[[biome4]], fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
th_we_bin <- int_map_biomes[[biome4]] >= th_we
th_we_bin <- terra::as.polygons(th_we_bin, dissolve = TRUE)
th_we_bin <- th_we_bin[th_we_bin$lyr.1 == 1, ]

par(mar = c(0, 0, 0, 0))
terra::plot(tropical_america_diss, pax = list(side = c(1), cex.axis = 2),
            main = paste("(d)", gsub("\\.", " ", biome4)), cex.main = 1.75)
terra::plot(int_map_biomes[[biome4]], 
            col = hcl.colors(n = 100, "RdYlBu", rev = T),
              # Use biome_name directly
            legend = FALSE,
            # xlab = "Longitude",
            # ylab = "Latitude",
            # cex.lab = 1.2,
            # pax = list(cex.axis = 1.1),
            add = TRUE)
terra::plot(th_we_bin, lwd = 2, add = TRUE)
terra::plot(america, add = TRUE)

# Shared y-axis name
# par(mar = c(0, 0, 0, 0))
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
mtext("Longitude", side = 3, line = -1.5, cex = 2)

dev.off()
}


# for(i in seq_along(my_order_biome_names)){
#   svg(paste0(here::here("figures"), "/", "S", i, "_", 
#              my_order_biome_names[i], ".svg"), width = 10, height = 13)
#   biome_name <- my_order_biome_names[i]
#   
#   th_we <- terra::global(int_map_biomes[[biome_name]], fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
#   th_we_bin <- int_map_biomes[[biome_name]] >= th_we
#   th_we_bin <- terra::as.polygons(th_we_bin, dissolve = TRUE)
#   th_we_bin <- th_we_bin[th_we_bin$lyr.1 == 1, ]
#   
#   par(mar = c(2, 3, 2, 2))
#   terra::plot(int_map_biomes[[biome_name]], 
#               col = hcl.colors(n = 100, "RdYlBu", rev = T),
#               main = gsub("\\.", " ", biome_name),  # Use biome_name directly
#               xlab = "Longitude",
#               ylab = "Latitude",
#               cex.lab = 1.5,
#               pax = list(cex.axis = 1.4))
#   terra::plot(th_we_bin, lwd = 3, add = T)
#   terra::plot(tropical_america_diss, add = TRUE)
#   dev.off()
# }

# Shared x-axis
par(mar = c(0, 0, 0, 0))
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
mtext("Longitude", side = 3, line = -4, cex = 4.5)

# Legend bar
par(mar = c(0, 0, 0, 5))
plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")
colramp <- hcl.colors(n = 100, "RdYlBu", rev = T)
rect(0.2, 0:99, 0.8, 1:100, col = colramp, border = NA)
axis(2, at = c(0, 100), labels = c("Low", "High"), las = 1, cex.axis = 4.5, line = -0.5)
# text(0.5, -5, "Value", xpd = TRUE, cex = 1.2)
dev.off()






# AREA INTERSECTION WITH HOTSPOTS AND PAs ---------------------------------

# How much top 90th quantile areas is inside each tropical american hotspot?
# Calculate area of each hotspot
hotspots$hotspot_area_km2 <- terra::expanse(hotspots, unit="km")
# Calculate intersection
intersection <- terra::intersect(th_we_bin_dis, hotspots)
# Calculate area of intersection for each hotspot polygon
intersection$intersected_area_km2 <- terra::expanse(intersection, unit="km")
# Aggregate intersected area by hotspot NAME
intersected_by_hotspot <- aggregate(
  intersection$intersected_area_km2,
  by = list(NAME = intersection$NAME),
  FUN = sum
)
# Merge with hotspot areas
result <- dplyr::left_join(intersected_by_hotspot, 
                unique(as.data.frame(hotspots)[, c("NAME", "hotspot_area_km2")]),
                by = "NAME")
result$percent_hotspot_covered <- (result$x / result$hotspot_area_km2) * 100
mean(result$percent_hotspot_covered)
writexl::write_xlsx(result, file.path(file_path3, "top90_per_hotspot.xlsx"))

##### How about protected areas? #####
wdpa_data <- terra::vect(here::here("data", "derived-data", 
                                    "maps", "tropicalAmerica_WDPA_Feb2026_cleaned.shp"))
wdpa_data <- terra::buffer(wdpa_data, 0)
# wdpa_data <- terra::fillHoles(terra::aggregate(wdpa_data, by = "SITE_ID", dissolve = TRUE, cores = 8))
wdpa_data <- terra::fillHoles(terra::aggregate(wdpa_data, dissolve = TRUE, cores = 8))
wdpa_data$allPA_area_km2 <- terra::expanse(wdpa_data, unit = "km")
intersection1 <- terra::intersect(th_we_bin_dis, wdpa_data)
intersection1$intersected_area_km2 <- terra::expanse(intersection1, unit = "km")
sum(intersection1$intersected_area_km2)
# Proportion of PA areas covering critical centers
100 * (sum(intersection1$intersected_area_km2) / sum(th_we_bin_area))
# 14.66%

# How much of tropical America is protected in %?
100 * (terra::expanse(wdpa_data, 'km') / 
         terra::expanse(tropical_america_diss, 'km')) # 7.35%

##### How about tropical American ecoregions inside each top 90th? #####
er <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_ecoregions.shp"))
er <- er[er$Morrone_re %in% "Neotropical", ]
er <- terra::buffer(er, 0)
er_nt <- terra::crop(er, tropical_america_diss)
er_nt <- terra::mask(er, tropical_america_diss)
er_nt <- terra::aggregate(er_nt, by = "ECO_NAME", cores = 6)
er_nt$er_area_km2 <- terra::expanse(er_nt, unit = "km")
er_we_int <- terra::intersect(th_we_bin_dis, er_nt)
er_we_int <- terra::aggregate(er_we_int, by = "ECO_NAME", cores = 6)
er_we_int$intersected_area_km2 <- terra::expanse(er_we_int, unit = "km")
er_we_int$prop_intersected_area_km2 <- 100 * (er_we_int$intersected_area_km2 / er_we_int$mean_er_area_km2)
# er_we_int_plot <- er_we_int[, c("ECO_NAME", "prop_intersected_area_km2")]
# 
# a = er_nt[er_nt$ECO_NAME %in% er_we_int$ECO_NAME, ]
# a$prop_for_plot <- er_we_int$prop_intersected_area_km2
# a$prop_for_plot_log <- log(a$prop_for_plot)
# 
# terra::plot(a[a$prop_for_plot > 50, ],
#             "prop_for_plot",
#             main = "Ecoregion proportion inside the CCE",
#             col = hcl.colors(n = 100, "RdYlBu", rev = T),
#             cex.main = 6,
#             pax = list(cex.axis = 5, las = 1, xaxt = "n", mgp = c(3, 2.5, 0)),
#             # legend = F,
#             axes = T)
# terra::plot(america, add = TRUE)

# Save this
er_we_int_df <- terra::as.data.frame(er_we_int)
wanted <- c("ECO_NAME", "intersected_area_km2", "mean_er_area_km2", "prop_intersected_area_km2")
er_we_int_df <- er_we_int_df[, wanted]
er_we_int_df$intersected_area_km2 <- round(er_we_int_df$intersected_area_km2, 2)
er_we_int_df$mean_er_area_km2 <- round(er_we_int_df$mean_er_area_km2, 2)
er_we_int_df$prop_intersected_area_km2 <- round(er_we_int_df$prop_intersected_area_km2, 2)
names(er_we_int_df) <- c("Ecoregion (Dinerstein et al. 2017)", "CCE area", "Ecoregion area", "Percentage")
writexl::write_xlsx(er_we_int_df, file.path(file_path3, "er_area_inside_cce.xlsx"))


# ##### EFG SUMMARY SCATTERPLOT #####
# library(ggplot2)
# library(ggpmisc)
# new_row <- data.frame(X = NA, 
#                       EFG = "Oceanic cool temperate rainforests",
#                       Richness = NA,
#                       Endemics = NA,
#                       Endemism_level = NA,
#                       EFG_area_km = NA)
# # Just to fix colors...
# scatt <- rbind(scatt, new_row)
# scatt <- scatt[order(scatt$EFG), ]
# cores <- MetroBrewer::metro.brewer("Beijing", n = 19)
# names(cores) <- scatt$EFG
# scatt$EFG <- factor(scatt$EFG, levels = names(cores))
# scatt1 <- scatt[!is.na(scatt$Richness), ]
# scatt1[is.na(scatt1)] <- 0
# scatt2 <- scatt1[scatt1$Endemics > 0, ]
# 
# 
# ggplot(scatt2, aes(x = Richness,
#                    y = Endemics,
#                    color = EFG)) +
#   geom_point(aes(size = Endemism_level), alpha = 0.7) + 
#   geom_smooth(method = "lm", 
#               se = TRUE,
#               color = "black",
#               linewidth = 0.5,
#               aes(group = 1),
#               show.legend = FALSE) +
#   ggpmisc::stat_poly_eq(formula = y ~ x,
#                         aes(label = paste(after_stat(rr.label), 
#                                           sep = "~~~")),
#                         label.x = "right",
#                         label.y = "top",
#                         size = 4,
#                         color = "black") +
#   scale_x_log10() +
#   scale_y_log10() +
#   scale_size_continuous(range = c(3, 15)) +
#   scale_color_manual(values = cores) +
#   labs(x = "\nSpecies richness (log10)",
#        y = "Endemic species (log10)\n",
#        color = "Biome",
#        size = "Endemism level (%)") +
#   theme_minimal() +
#   theme(legend.position = "right",
#         axis.title = element_text(size = 15),
#         axis.text = element_text(size = 13), 
#         legend.title = element_text(size = 13),
#         legend.text = element_text(size = 13))
# 
# ggsave("figures/FigureS2.svg", device = "svg",
#        height = 7, width = 12)


