
# Plots and figures and stuff ---------------------------------------------

# Load necessary data
file_path0 <- here::here("data", "derived-data")
file_path1 <- here::here("data", "derived-data", "rds")
file_path2 <- here::here("data", "derived-data", "maps")
file_path3 <- here::here("outputs", "tables")
file_path4 <- here::here("data", 'raw-data', "maps")
file1 <- "end_per_efg.rds"
file2 <- "spp_per_efg.rds"
# Substitute here later for 1 km map
file4 <- "interpolated_maps_50km.tiff"
file44 <- "_biomes_"
file5 <- "final_df.rds"
file6 <- "missing_trees.rds"
file7 <- "checklist_species_level.rds"
# file8 <- "we_map_area_50km_null_model.rds"
file9 <- "efg_summary_table.xlsx"
file10 <- "hotspots_2016_1.shp"

end_efg <- readRDS(file.path(file_path1, file1))
spp_efg <- readRDS(file.path(file_path1, file2))
# 0.5 degree grid of the WE map (06), used to rasterize the records (Figure S3)
we_map_area <- readRDS(file.path(file_path1, "we_map_area_50km.rds"))
wee_map <- terra::rast(we_map_area$WE_raster, crs = "epsg:4326")
terra::values(wee_map) <- raster::values(we_map_area$WE_raster)
rm(we_map_area)
int_map <- terra::rast(file.path(file_path2, file4))
# biomes...
all_files <- list.files(file_path2, full.names = TRUE)
biome_files <- all_files[grepl("_biomes_50km_", all_files)]
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
my_df1 <- my_df[, c("decimalLongitude.new1", "decimalLatitude.new1")]
my_df1$decimalLongitude.new1 <- as.numeric(my_df1$decimalLongitude.new1)
my_df1$decimalLatitude.new1 <- as.numeric(my_df1$decimalLatitude.new1)
my_df1 <- terra::vect(my_df1, 
                      geom = c("decimalLongitude.new1",
                               "decimalLatitude.new1"),
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


# GET codes from IUCN, looked up by EFG name (new EFGs do not shift the codes)
getCodes <- c("Cool deserts and semi-deserts" = "T5.4",
              "Hyper-arid deserts" = "T5.5",
              "Intertidal forests and shrublands" = "MFT1.2",
              "Oceanic cool temperate rainforests" = "T2.3",
              "Pyric tussock savannas" = "T4.2",
              "Rocky pavements, screes and lava flows" = "T3.4",
              "Seasonal floodplain marshes" = "TF1.4",
              "Seasonally dry temperate heaths and shrublands" = "T3.2",
              "Seasonally dry tropical shrublands" = "T3.1",
              "Semi-desert steppes" = "T5.1",
              "Temperate alpine grasslands and shrublands" = "T6.4",
              "Temperate subhumid grasslands" = "T4.5",
              "Temperate woodlands" = "T4.4",
              "Thorny deserts and semi-deserts" = "T5.2",
              "Tropical alpine grasslands and herbfields" = "T6.5",
              "Tropical flooded forests and peat forests" = "TF1.1",
              "Tropical heath forests" = "T1.4",
              "Tropical-subtropical dry forests and thickets" = "T1.2",
              "Tropical-subtropical lowland rainforests" = "T1.1",
              "Tropical-subtropical montane rainforests" = "T1.3",
              "Warm temperate laurophyll forests" = "T2.4")
# Add the codes to the names (an EFG missing above shows up as "(NA)")
tropical_america$our_efg <- paste0(tropical_america$our_efg, " ", "(", getCodes[tropical_america$our_efg], ")")

tropical_america_diss <- terra::aggregate(tropical_america, cores = 8)
tropical_america_diss <- terra::fillHoles(tropical_america_diss)
# Hotspots nt 2016
hotspots <- terra::vect(file.path(file_path4, file10))
# Only hotspot area, not outer limits
hotspots <- hotspots[hotspots$Type %in% "hotspot area", ]
hotspots <- terra::crop(hotspots, tropical_america_diss)
hotspots <- terra::mask(hotspots, tropical_america_diss)

# WDPA for tropical America: prepared by 00_prepare_wdpa.R

# Whole
america <- terra::aggregate(america, cores = 8)
america <- terra::fillHoles(america)

# Figure style (same as Assessments/R/figure_style.R) ---------------------
proj_m <- "+proj=aea +lat_1=10 +lat_2=-20 +lat_0=-5 +lon_0=-65 +datum=WGS84 +units=m +no_defs"
pal_div <- hcl.colors(100, "RdYlBu", rev = TRUE)
col_lo  <- pal_div[1]
col_hi  <- pal_div[100]
fig_w     <- 10
fig_h     <- 8
fig_h_map <- 10
# Outline of the Americas and map limits (tropical America), in proj_m
america_sf <- sf::st_as_sf(terra::project(america, proj_m))
ta_box <- sf::st_bbox(sf::st_as_sf(terra::project(tropical_america_diss, proj_m)))
# Theme for every figure
theme_sts <- ggplot2::theme_bw(base_size = 15) +
  ggplot2::theme(panel.grid = ggplot2::element_line(colour = "grey90", linewidth = 0.2),
                 legend.position = "right",
                 legend.key = ggplot2::element_blank(),
                 strip.background = ggplot2::element_rect(fill = "grey95"))

map_sts <- list(
  ggplot2::geom_sf(data = america_sf, fill = NA, colour = "black", linewidth = 0.3),
  ggspatial::annotation_scale(location = "br", width_hint = 0.2, text_cex = 1, style = "bar"),
  ggplot2::coord_sf(crs = proj_m, xlim = ta_box[c("xmin", "xmax")] + c(-2e5, 2e5),
                    ylim = ta_box[c("ymin", "ymax")] + c(-2e5, 2e5), expand = FALSE),
  ggplot2::labs(x = "Longitude", y = "Latitude")
)
# Interpolated maps (with the null-model)
int_map_df <- terra::as.data.frame(terra::project(int_map, proj_m, res = 50000, method = "near"),
                                   xy = TRUE, na.rm = FALSE)

# Preparing the study region map ------------------------------------------
cores <- c(T5.4 = "#FF9A9A", T5.5 = "#CCAD60", MFT1.2 = "#008E9C", T2.3 = "#A61C80",
           T4.2 = "#ED2621", T3.4 = "#4A4A4A", TF1.4 = "#964B00", T3.2 = "#C2A5CF",
           T3.1 = "#8EC31F", T5.1 = "#E3A857", T6.4 = "#009BC0", T4.5 = "#ED796A",
           T4.4 = "#2D9EE0", T5.2 = "#D5A7A1", T6.5 = "#6A357E", TF1.1 = "#253D95",
           T1.4 = "#F368A1", T1.2 = "#FDCF07", T1.1 = "#098137", T1.3 = "#94989B",
           T2.4 = "#960153")
cores <- cores[sub("^.* \\((.+)\\)$", "\\1", tropical_america$our_efg)]
names(cores) <- tropical_america$our_efg
tropical_america$our_efg <- factor(tropical_america$our_efg, levels = names(cores))

p_fig1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = sf::st_as_sf(tropical_america), ggplot2::aes(fill = our_efg),
                   colour = NA) +
  map_sts +
  ggplot2::scale_fill_manual(name = NULL, values = cores, drop = FALSE) +
  ggplot2::guides(fill = ggplot2::guide_legend(ncol = 1)) +
  theme_sts +
  ggplot2::theme(legend.position = "inside",
                 legend.position.inside = c(0.01, 0.01),
                 legend.justification = c(0, 0),
                 legend.background = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 10),
                 legend.key.size = ggplot2::unit(0.9, "lines"))
ggplot2::ggsave(here::here("figures", "Figure1.svg"), p_fig1, width = fig_w, height = fig_h_map)
# PDF (vector) for PNAS
ggplot2::ggsave(here::here("figures", "Figure1.pdf"), p_fig1, width = fig_w, height = fig_h_map)
ggplot2::ggsave(here::here("figures", "Figure1.png"), p_fig1, width = fig_w, height = fig_h_map, dpi = 300)


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
# Commented out: 07_rarefy_interpolate.R no longer produces krig.simp.obs
# th_simp.obs <- terra::global(int_map$krig.simp.obs, fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
# th_simp.obs_bin <- int_map$krig.simp.obs > th_simp.obs
# th_simp.obs_bin <- terra::as.polygons(th_simp.obs_bin, dissolve = TRUE)
# # Keep only 1
# th_simp.obs_bin <- th_simp.obs_bin[th_simp.obs_bin$krig.simp.obs == 1, ]
# # Disaggregate to measure polygon sizes
# th_simp.obs_bin_dis <- terra::disagg(th_simp.obs_bin)
# th_simp.obs_bin_area <- terra::expanse(th_simp.obs_bin_dis, unit = "km")
# th_simp.obs_bin_dis$area_km2 <- round(th_simp.obs_bin_area, 1)
# # Keeping only > 50 km² polygons to ease big pattern visualization and also since
# # this is our base area
# th_simp.obs_bin_dis <- th_simp.obs_bin_dis[th_simp.obs_bin_dis$area_km2 > 50, ]
# terra::plot(int_map$krig.simp.obs, main = "xxx", col = hcl.colors(n = 100, "RdYlBu", rev = T))
# terra::plot(th_simp.obs_bin_dis, lwd = 3, add = T)

# Make layout
# my_lay <- matrix(c(1, 2, 3, 7, 1, 4, 5, 7, 6, 6, 6, 6),
#                  nrow = 3, ncol = 4, byrow = TRUE)




# New figures: Figure 2, Figure S3, Figure S4, Figure S5, Figure S7 -----------------

##### Figure 2 #####
# krig.we is already back in WE units (09); the legend only shows low/high
# because raw WE values (~1e-10) mean little to the reader
we_df <- int_map_df[!is.na(int_map_df$krig.we), ]
p_we <- ggplot2::ggplot() +
  ggplot2::geom_tile(data = we_df, ggplot2::aes(x, y, fill = krig.we),
                     width = 50000, height = 50000) +
  ggplot2::geom_sf(data = sf::st_as_sf(th_we_bin_dis[th_we_bin_dis$area_km2 > 50, ]),
                   ggplot2::aes(linetype = "Top 10%"), fill = NA, colour = "black", linewidth = 0.7) +
  map_sts +
  ggplot2::scale_fill_gradientn(name = "Weighted\nendemism", colours = pal_div, trans = "log10",
                                breaks = range(we_df$krig.we), labels = c("Low", "High")) +
  ggplot2::scale_linetype_manual(name = NULL, values = c("Top 10%" = "solid")) +
  ggplot2::guides(fill = ggplot2::guide_colourbar(order = 1, barheight = ggplot2::unit(12, "lines")),
                  linetype = ggplot2::guide_legend(order = 2)) +
  ggplot2::labs(title = "Weighted endemism") +
  theme_sts
# Inset: kriged WE vs sampling effort (cells with records), CCE highlighted.
# Spearman rho (kriged and raw WE vs N) goes in the figure legend
eff_df <- int_map_df[!is.na(int_map_df$N) & !is.na(int_map_df$krig.we), ]
eff_df$cce <- eff_df$krig.we >= stats::quantile(int_map_df$krig.we, 0.9, na.rm = TRUE)
p_inset <- ggplot2::ggplot(eff_df, ggplot2::aes(N, krig.we)) +
  ggplot2::geom_point(ggplot2::aes(colour = cce), size = 0.5, alpha = 0.6, show.legend = FALSE) +
  ggplot2::scale_colour_manual(values = c(`FALSE` = "grey60", `TRUE` = col_hi)) +
  ggplot2::scale_x_log10() + ggplot2::scale_y_log10(labels = NULL) +
  ggplot2::labs(x = "Records per cell", y = "WE") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(panel.grid = ggplot2::element_blank(),
                 plot.background = ggplot2::element_rect(fill = "white", colour = "grey40"))
# Lower-left of the map (Pacific), 200 km inside the panel edges, in the projected units of map_sts
x0 <- ta_box[["xmin"]]
y0 <- ta_box[["ymin"]]
dx <- 0.30 * (ta_box[["xmax"]] - ta_box[["xmin"]])
dy <- 0.24 * (ta_box[["ymax"]] - ta_box[["ymin"]])
p_we <- p_we + ggplot2::annotation_custom(ggplot2::ggplotGrob(p_inset),
                                          xmin = x0, xmax = x0 + dx, ymin = y0, ymax = y0 + dy)
rm(eff_df, p_inset, x0, y0, dx, dy)
ggplot2::ggsave(here::here("figures", "Figure2.svg"), p_we, width = fig_w, height = fig_h_map)
# PDF (vector) for PNAS
ggplot2::ggsave(here::here("figures", "Figure2.pdf"), p_we, width = fig_w, height = fig_h_map)
ggplot2::ggsave(here::here("figures", "Figure2.png"), p_we, width = fig_w, height = fig_h_map, dpi = 300)

##### Figure S5 #####
# krig.s.obs is already back in number of species (09)
p_rich <- ggplot2::ggplot() +
  ggplot2::geom_tile(data = int_map_df[!is.na(int_map_df$krig.s.obs), ],
                     ggplot2::aes(x, y, fill = krig.s.obs), width = 50000, height = 50000) +
  map_sts +
  ggplot2::scale_fill_gradientn(name = "Species", colours = pal_div, trans = "log10") +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = ggplot2::unit(12, "lines"))) +
  ggplot2::labs(title = "Endemic species richness") +
  theme_sts
ggplot2::ggsave(here::here("figures", "FigureS5.svg"), p_rich, width = fig_w, height = fig_h_map)
ggplot2::ggsave(here::here("figures", "FigureS5.png"), p_rich, width = fig_w, height = fig_h_map, dpi = 300)
# PDF (vector) for PNAS
ggplot2::ggsave(here::here("figures", "FigureS5.pdf"), p_rich, width = fig_w, height = fig_h_map)

##### Figure S3 #####
dens_df <- terra::as.data.frame(terra::project(dens, proj_m, res = 50000, method = "near"), xy = TRUE)
p_dens <- ggplot2::ggplot() +
  ggplot2::geom_tile(data = dens_df, ggplot2::aes(x, y, fill = sum), width = 50000, height = 50000) +
  map_sts +
  ggplot2::scale_fill_gradientn(name = "Records\nper cell", colours = pal_div, trans = "log10") +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = ggplot2::unit(12, "lines"))) +
  ggplot2::labs(title = "Sampling effort") +
  theme_sts
ggplot2::ggsave(here::here("figures", "FigureS3.svg"), p_dens, width = fig_w, height = fig_h_map)
ggplot2::ggsave(here::here("figures", "FigureS3.png"), p_dens, width = fig_w, height = fig_h_map, dpi = 300)
# PDF (vector) for PNAS
ggplot2::ggsave(here::here("figures", "FigureS3.pdf"), p_dens, width = fig_w, height = fig_h_map)

##### Figure S4: top municipalities #####
# (a) The ten most collected municipalities (Hans ter Steege's comment), numbered as in
# top_collected_municipalities.xlsx; (b) and (c) zooms on overlapping ranks.
# Records inside tropical America (same records as the density map); municipality = first
# three levels of loc.correct1 (country_state_municipality). NAME_all in latamMap has four
# levels in Brazil, Peru, Bolivia and Ecuador (districts), so map units are matched on their
# first three levels and dissolved. Records resolved only to country or state are not counted.
in_ta <- !is.na(terra::extract(dens, my_df1)[, 2])
muni <- my_df$loc.correct1[in_ta]
muni_xy <- terra::crds(my_df1)[in_ta, ]
keep <- lengths(strsplit(muni, "_")) >= 3
muni <- sub("^([^_]+_[^_]+_[^_]+).*$", "\\1", muni[keep])
muni_xy <- muni_xy[keep, ]
muni_n <- as.data.frame(table(NAME_all = muni), stringsAsFactors = FALSE)
names(muni_n)[2] <- "n_records"
# Ten most collected municipalities (Table S4)
top_muni <- muni_n[order(muni_n$n_records, decreasing = TRUE)[1:10], ]
top_muni$rank <- seq_len(nrow(top_muni))
top_muni$pct_records <- 100 * top_muni$n_records / sum(in_ta)
top_muni$Country <- tools::toTitleCase(sapply(strsplit(top_muni$NAME_all, "_"), `[`, 1))
top_muni$State <- tools::toTitleCase(sapply(strsplit(top_muni$NAME_all, "_"), `[`, 2))
top_muni$Municipality <- tools::toTitleCase(sapply(strsplit(top_muni$NAME_all, "_"), `[`, 3))
writexl::write_xlsx(top_muni[, c("rank", "Municipality", "State", "Country", "n_records", "pct_records")],
                    file.path(file_path3, "top_collected_municipalities.xlsx"))
# Polygons of the ten municipalities (map units dissolved to three levels)
latam_muni <- terra::vect(file.path(file_path2, "latamMap_classified.gpkg"))
latam_muni$NAME_3lev <- sub("^([^_]+_[^_]+_[^_]+).*$", "\\1", latam_muni$NAME_all)
latam_muni <- latam_muni[latam_muni$NAME_3lev %in% top_muni$NAME_all, "NAME_3lev"]
latam_muni <- terra::aggregate(latam_muni, by = "NAME_3lev")
latam_muni <- terra::simplifyGeom(terra::project(latam_muni, proj_m), tolerance = 1000)
latam_muni <- sf::st_as_sf(latam_muni)
# Label positions: point inside the polygon; median coordinates of the records if no polygon
lab <- top_muni[, c("rank", "NAME_all")]
lab_xy <- t(sapply(lab$NAME_all, function(m) apply(muni_xy[muni == m, , drop = FALSE], 2, median)))
lab_xy <- terra::crds(terra::project(terra::vect(lab_xy, crs = "epsg:4326"), proj_m))
in_map <- match(lab$NAME_all, latam_muni$NAME_3lev)
lab_xy[!is.na(in_map), ] <- sf::st_coordinates(sf::st_point_on_surface(latam_muni[in_map[!is.na(in_map)], ]))
lab$x <- lab_xy[, 1]
lab$y <- lab_xy[, 2]

# Zoom areas A (ranks 7-8) and B (ranks 9-10), which overlap in (a): square box around the
# polygons and label points, padded by max(30 km, 25% of the larger side), at least 300 km wide
# so the rectangle is visible on the full map
zoom_ids <- list(c(7, 8), c(9, 10))
zoom_box <- do.call(rbind, lapply(seq_along(zoom_ids), function(i) {
  sel <- lab$rank %in% zoom_ids[[i]]
  bb <- sf::st_bbox(latam_muni[latam_muni$NAME_3lev %in% lab$NAME_all[sel], ])
  xr <- range(c(bb[c("xmin", "xmax")], lab$x[sel]), na.rm = TRUE)
  yr <- range(c(bb[c("ymin", "ymax")], lab$y[sel]), na.rm = TRUE)
  side <- max(diff(xr), diff(yr))
  half <- max(side / 2 + max(3e4, 0.25 * side), 1.5e5)
  data.frame(id = LETTERS[i], xmin = mean(xr) - half, xmax = mean(xr) + half,
             ymin = mean(yr) - half, ymax = mean(yr) + half)
}))
p_muni <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = latam_muni, fill = "#b2182b", colour = NA) +
  ggplot2::geom_point(data = lab, ggplot2::aes(x, y), shape = 21, size = 6,
                      stroke = 1, colour = "black", fill = NA) +
  # Ranks inside the zoom boxes are numbered in (b) and (c) only
  ggplot2::geom_text(data = lab[!lab$rank %in% unlist(zoom_ids), ], ggplot2::aes(x, y, label = rank),
                     nudge_x = 1.8e5, size = 5, fontface = "bold") +
  ggplot2::geom_rect(data = zoom_box, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                     fill = NA, colour = "black", linewidth = 0.4) +
  ggplot2::geom_text(data = zoom_box, ggplot2::aes(xmin, ymax, label = id),
                     hjust = 1.2, vjust = -0.2, size = 5, fontface = "bold") +
  map_sts +
  ggplot2::labs(title = "(a) Ten most collected municipalities") +
  theme_sts
# Insets (b) and (c): same layers as (a), limited to the zoom boxes
p_zoom <- lapply(seq_len(nrow(zoom_box)), function(i) {
  ggplot2::ggplot() +
    map_sts[[1]] +
    ggplot2::geom_sf(data = latam_muni, fill = "#b2182b", colour = NA) +
    ggplot2::geom_point(data = lab, ggplot2::aes(x, y), shape = 21, size = 6,
                        stroke = 1, colour = "black", fill = NA) +
    ggplot2::geom_text(data = lab, ggplot2::aes(x, y, label = rank),
                       nudge_x = 0.1 * (zoom_box$xmax[i] - zoom_box$xmin[i]), size = 5, fontface = "bold") +
    ggspatial::annotation_scale(location = "br", width_hint = 0.3, text_cex = 0.7, style = "bar",
                                height = ggplot2::unit(0.15, "cm"), pad_y = ggplot2::unit(0.3, "cm")) +
    ggplot2::coord_sf(crs = proj_m, xlim = c(zoom_box$xmin[i], zoom_box$xmax[i]),
                      ylim = c(zoom_box$ymin[i], zoom_box$ymax[i]), expand = FALSE) +
    ggplot2::labs(title = paste0("(", letters[1 + i], ") Zoom ", zoom_box$id[i], ": ranks ",
                                 paste(zoom_ids[[i]], collapse = " and "))) +
    theme_sts +
    ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(), plot.title = ggplot2::element_text(size = 13))
})
# Legend of the ten municipalities
leg_txt <- paste0(top_muni$rank, ". ", top_muni$Municipality, ", ", top_muni$State, " (", top_muni$Country,
                  ") - N = ", format(top_muni$n_records, big.mark = ",", trim = TRUE))
p_leg <- ggplot2::ggplot() +
  ggplot2::annotate("text", x = 0, y = rev(seq_along(leg_txt)), label = leg_txt, hjust = 0, size = 4) +
  ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
  ggplot2::scale_y_continuous(limits = c(0.5, length(leg_txt) + 0.5)) +
  ggplot2::coord_cartesian(clip = "off") +
  ggplot2::labs(title = "Municipalities") +
  ggplot2::theme_void(base_size = 15) +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
p_top <- patchwork::wrap_plots(p_muni, patchwork::wrap_plots(p_zoom[[1]], p_zoom[[2]], p_leg, ncol = 1),
                               ncol = 2, widths = c(1, 0.8))
ggplot2::ggsave(here::here("figures", "FigureS4.svg"), p_top, width = 1.8 * fig_w, height = fig_h_map)
ggplot2::ggsave(here::here("figures", "FigureS4.png"), p_top, width = 1.8 * fig_w, height = fig_h_map,
                dpi = 300)
# PDF (vector) for PNAS
ggplot2::ggsave(here::here("figures", "FigureS4.pdf"), p_top, width = 1.8 * fig_w, height = fig_h_map)


##### Figure S7: WE null model #####
# (a) observed WE minus the null median, in units of the null interquartile
# range (squished at +-3); (b) one-sided test, P <= 0.025 higher and
# P >= 0.975 lower than expected; (c) outliers beyond 1.5 x IQR of the null.
# (b) and (c) share the same three classes, so one legend serves both
null_df <- int_map_df[!is.na(int_map_df$we_null_P_above), ]
null_df$P_class <- cut(null_df$we_null_P_above, breaks = c(0, 0.025, 0.975, 1),
                       labels = c("Higher", "As expected", "Lower"), include.lowest = TRUE)
null_df$out_class <- factor(null_df$we_null_out_above_below, levels = c(1, 0, -1),
                            labels = c("Higher", "As expected", "Lower"))
null_cols <- c("Higher" = col_hi, "As expected" = "grey85", "Lower" = col_lo)
p_null_a <- ggplot2::ggplot() +
  ggplot2::geom_tile(data = null_df, ggplot2::aes(x, y, fill = we_null_out_continuous),
                     width = 50000, height = 50000) +
  map_sts +
  ggplot2::scale_fill_gradientn(name = "Deviation from\nnull (IQR)", colours = pal_div,
                                limits = c(-3, 3), oob = scales::squish, breaks = c(-3, -1.5, 0, 1.5, 3),
                                labels = c("\u2264 -3", "-1.5", "0", "1.5", "\u2265 3")) +
  ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = ggplot2::unit(10, "lines"))) +
  ggplot2::labs(title = "(a) Deviation") +
  theme_sts
p_null_b <- ggplot2::ggplot() +
  ggplot2::geom_tile(data = null_df, ggplot2::aes(x, y, fill = P_class), width = 50000, height = 50000) +
  map_sts +
  ggplot2::scale_fill_manual(name = "WE vs. null", values = null_cols, drop = FALSE) +
  ggplot2::labs(title = "(b) Significance") +
  theme_sts
p_null_c <- ggplot2::ggplot() +
  ggplot2::geom_tile(data = null_df, ggplot2::aes(x, y, fill = out_class), width = 50000, height = 50000) +
  map_sts +
  ggplot2::scale_fill_manual(name = "WE vs. null", values = null_cols, drop = FALSE) +
  ggplot2::labs(title = "(c) Outliers") +
  theme_sts
p_null <- cowplot::plot_grid(
  p_null_a + ggplot2::theme(legend.position = "none"),
  p_null_b + ggplot2::theme(legend.position = "none"),
  p_null_c + ggplot2::theme(legend.position = "none"),
  cowplot::plot_grid(cowplot::get_legend(p_null_a), cowplot::get_legend(p_null_b), ncol = 1),
  ncol = 2)
ggplot2::ggsave(here::here("figures", "FigureS7.svg"), p_null, width = fig_w, height = fig_h_map, bg = "white")
ggplot2::ggsave(here::here("figures", "FigureS7.png"), p_null, width = fig_w, height = fig_h_map, dpi = 300, bg = "white")
# PDF (vector) for PNAS
ggplot2::ggsave(here::here("figures", "FigureS7.pdf"), p_null, width = fig_w, height = fig_h_map, bg = "white")

# WE vs sampling effort (numbers for the CCE text, Moabe's comment) -------
we_eff <- terra::as.data.frame(int_map[[c("we", "N", "krig.we", "we_null_out_above_below")]],
                               na.rm = FALSE)
# CCE threshold over all interpolated cells; comparisons over cells with records
we_eff$cce <- we_eff$krig.we >= stats::quantile(terra::values(int_map$krig.we), 0.9, na.rm = TRUE)
we_eff <- we_eff[!is.na(we_eff$N) & !is.na(we_eff$krig.we), ]
we_eff$top_n <- we_eff$N >= stats::quantile(we_eff$N, 0.9)
we_vs_effort <- data.frame(
  metric = c("rho_rawWE_N", "rho_krigWE_N", "pct_CCE_in_top10_N",
             "median_N_CCE", "median_N_other",
             "pct_CCE_above_null", "pct_top10_N_above_null", "pct_all_above_null"),
  value = c(stats::cor(we_eff$we, we_eff$N, method = "spearman"),
            stats::cor(we_eff$krig.we, we_eff$N, method = "spearman"),
            100 * mean(we_eff$top_n[we_eff$cce]),
            stats::median(we_eff$N[we_eff$cce]),
            stats::median(we_eff$N[!we_eff$cce]),
            100 * mean(we_eff$we_null_out_above_below[we_eff$cce] %in% 1),
            100 * mean(we_eff$we_null_out_above_below[we_eff$top_n] %in% 1),
            100 * mean(we_eff$we_null_out_above_below %in% 1)))
writexl::write_xlsx(we_vs_effort, here::here("outputs", "tables", "we_vs_effort.xlsx"))
rm(we_eff)

# WE vs species richness (Hans ter Steege's comment) ----------------------
# Herbarium records only: same records (no realm transitional/predominant)
# and same 50 km cells as the WE map. No plot data.
occ <- readRDS(here::here("data", "derived-data", "rds", "final_df.rds"))
occ <- occ[occ$is_realm_predominant %in% FALSE & occ$is_realm_transitional %in% FALSE, ]
occ$cell <- terra::cellFromXY(int_map, cbind(as.numeric(occ$decimalLongitude.new1),
                                              as.numeric(occ$decimalLatitude.new1)))
occ <- occ[!is.na(occ$cell), c("cell", "scientificName")]
# Observed and rarefied richness per cell (Hurlbert 1971; n = 20 records, as base.n in 07)
we_rich <- do.call(rbind, lapply(split(occ$scientificName, occ$cell), function(sp) {
  ni <- as.vector(table(sp))
  data.frame(N_rec = sum(ni),
             S_obs = length(ni),
             S_rare = if (sum(ni) >= 20) sum(1 - exp(lchoose(sum(ni) - ni, 20) - lchoose(sum(ni), 20))) else NA_real_)
}))
we_rich$cell <- as.numeric(rownames(we_rich))
we_rich <- cbind(we_rich, terra::extract(int_map[["we"]], we_rich$cell))
we_rich <- we_rich[!is.na(we_rich$we), ]
# Corrected weighted endemism (WE / S): endemism not explained by richness alone
we_rich$cwe <- we_rich$we / we_rich$S_obs
# Hotspot overlap over all interpolated cells (top 10%, same threshold as the CCE)
krig <- terra::as.data.frame(int_map[[c("krig.we", "krig.s.obs")]], na.rm = TRUE)
krig$top_we <- krig$krig.we >= stats::quantile(krig$krig.we, 0.9)
krig$top_s <- krig$krig.s.obs >= stats::quantile(krig$krig.s.obs, 0.9)
we_vs_richness <- data.frame(
  metric = c("n_cells_raw", "n_cells_rarefied",
             "rho_rawWE_Sobs", "rho_rawWE_Srare20", "rho_krigWE_krigSobs",
             "rho_CWE_Srare20", "rho_Srare20_Nrec", "rho_Sobs_Nrec",
             "pct_CCE_in_top10_krigS"),
  value = c(nrow(we_rich), sum(!is.na(we_rich$S_rare)),
            stats::cor(we_rich$we, we_rich$S_obs, method = "spearman"),
            stats::cor(we_rich$we, we_rich$S_rare, method = "spearman", use = "complete.obs"),
            stats::cor(krig$krig.we, krig$krig.s.obs, method = "spearman"),
            stats::cor(we_rich$cwe, we_rich$S_rare, method = "spearman", use = "complete.obs"),
            stats::cor(we_rich$S_rare, we_rich$N_rec, method = "spearman", use = "complete.obs"),
            stats::cor(we_rich$S_obs, we_rich$N_rec, method = "spearman"),
            100 * mean(krig$top_s[krig$top_we])))
writexl::write_xlsx(we_vs_richness, here::here("outputs", "tables", "we_vs_richness.xlsx"))
rm(occ, we_rich, krig)

# CCE in the Cerrado and the Caatinga (numbers for Kyle's comment) --------
ecoreg <- terra::vect(file.path(file_path2, "NTAmericas_ecoregions.shp"))
# Biome-level map (Figure 3) used for each ecoregion
eco_biome <- c(Cerrado = "Pyric.tussock.savannas",
               Caatinga = "Tropical.subtropical.dry.forests.and.thickets")
cce_eco <- do.call(rbind, lapply(names(eco_biome), function(eco) {
  # Figure 2: CCE of all tropical American endemics (top 10% of krig.we)
  we_ta <- terra::values(int_map$krig.we)[, 1]
  in_eco <- terra::values(terra::rasterize(ecoreg[ecoreg$ECO_NAME %in% eco], int_map$krig.we,
                                           background = 0))[, 1] == 1
  cce_ta <- we_ta >= stats::quantile(we_ta, 0.9, na.rm = TRUE)
  # Figure 3: CCE of the biome endemics, and its patches inside the ecoregion
  we_b <- int_map_biomes[[eco_biome[eco]]]$krig.we
  cce_b <- we_b >= stats::quantile(terra::values(we_b), 0.9, na.rm = TRUE)
  cce_b_eco <- cce_b & terra::rasterize(ecoreg[ecoreg$ECO_NAME %in% eco], we_b, background = 0) == 1
  n_cce_b_eco <- sum(terra::values(cce_b_eco), na.rm = TRUE)
  data.frame(ecoregion = eco,
             pct_cells_TA = 100 * sum(in_eco & !is.na(we_ta)) / sum(!is.na(we_ta)),
             n_CCE_TA = sum(in_eco & cce_ta, na.rm = TRUE),
             pct_CCE_TA = 100 * sum(in_eco & cce_ta, na.rm = TRUE) / sum(cce_ta, na.rm = TRUE),
             n_CCE_biome = n_cce_b_eco,
             pct_CCE_biome = 100 * n_cce_b_eco / sum(terra::values(cce_b), na.rm = TRUE),
             n_patches_biome = terra::global(terra::patches(cce_b_eco, zeroAsNA = TRUE),
                                             "max", na.rm = TRUE)[1, 1])
}))
writexl::write_xlsx(cce_eco, here::here("outputs", "tables", "cce_cerrado_caatinga.xlsx"))
rm(ecoreg, eco_biome)


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


# Figure 3: WE inside each interpolated biome -----------------------------
# Each panel has its own colour scale (low/high within the biome); black lines
# mark the top 10% WE cells of each biome
p_biomes <- lapply(seq_along(biome_names), function(i) {
  r <- int_map_biomes[[biome_names[i]]][["krig.we"]]
  top <- terra::as.polygons(r >= terra::global(r, quantile, probs = 0.90, na.rm = TRUE)[1, 1],
                            dissolve = TRUE)
  top <- top[top$krig.we == 1, ]
  r_df <- terra::as.data.frame(terra::project(r, proj_m, res = 50000, method = "near"), xy = TRUE)
  # Short title: "Tropical.subtropical.lowland.rainforests" -> "Lowland rainforests"
  ttl <- gsub("\\.", " ", sub("^Tropical.subtropical.", "", biome_names[i]))
  ttl <- paste0("(", letters[i], ") ", toupper(substr(ttl, 1, 1)), substring(ttl, 2))
  ggplot2::ggplot() +
    ggplot2::geom_tile(data = r_df, ggplot2::aes(x, y, fill = krig.we), width = 50000, height = 50000) +
    ggplot2::geom_sf(data = sf::st_as_sf(top), fill = NA, colour = "black", linewidth = 0.5) +
    map_sts +
    ggplot2::scale_fill_gradientn(name = "Weighted\nendemism", colours = pal_div, trans = "log10",
                                  breaks = range(r_df$krig.we), labels = c("Low", "High")) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = ggplot2::unit(12, "lines"))) +
    ggplot2::labs(title = ttl) +
    theme_sts
})
p_fig3 <- cowplot::plot_grid(
  cowplot::plot_grid(plotlist = lapply(p_biomes, `+`, ggplot2::theme(legend.position = "none")),
                     ncol = 2),
  cowplot::get_legend(p_biomes[[1]]),
  rel_widths = c(1, 0.15))
ggplot2::ggsave(here::here("figures", "Figure3.svg"), p_fig3, width = fig_w, height = fig_h_map, bg = "white")
# PDF (vector) for PNAS
ggplot2::ggsave(here::here("figures", "Figure3.pdf"), p_fig3, width = fig_w, height = fig_h_map, bg = "white")
ggplot2::ggsave(here::here("figures", "Figure3.png"), p_fig3, width = fig_w, height = fig_h_map, dpi = 300, bg = "white")

# Figure S6: richness of endemics inside each interpolated biome ----------
# Each panel has its own colour scale (low/high within the biome)
p_rich_biomes <- lapply(seq_along(biome_names), function(i) {
  r <- int_map_biomes[[biome_names[i]]][["krig.s.obs"]]
  r_df <- terra::as.data.frame(terra::project(r, proj_m, res = 50000, method = "near"), xy = TRUE)
  ttl <- gsub("\\.", " ", sub("^Tropical.subtropical.", "", biome_names[i]))
  ttl <- paste0("(", letters[i], ") ", toupper(substr(ttl, 1, 1)), substring(ttl, 2))
  ggplot2::ggplot() +
    ggplot2::geom_tile(data = r_df, ggplot2::aes(x, y, fill = krig.s.obs), width = 50000, height = 50000) +
    map_sts +
    ggplot2::scale_fill_gradientn(name = "Endemic\nspecies", colours = pal_div, trans = "log10",
                                  breaks = range(r_df$krig.s.obs), labels = c("Low", "High")) +
    ggplot2::guides(fill = ggplot2::guide_colourbar(barheight = ggplot2::unit(12, "lines"))) +
    ggplot2::labs(title = ttl) +
    theme_sts
})
p_figS6 <- cowplot::plot_grid(
  cowplot::plot_grid(plotlist = lapply(p_rich_biomes, `+`, ggplot2::theme(legend.position = "none")),
                     ncol = 2),
  cowplot::get_legend(p_rich_biomes[[1]]),
  rel_widths = c(1, 0.15))
ggplot2::ggsave(here::here("figures", "FigureS6.svg"), p_figS6, width = fig_w, height = fig_h_map, bg = "white")
ggplot2::ggsave(here::here("figures", "FigureS6.png"), p_figS6, width = fig_w, height = fig_h_map, dpi = 300, bg = "white")
# PDF (vector) for PNAS
ggplot2::ggsave(here::here("figures", "FigureS6.pdf"), p_figS6, width = fig_w, height = fig_h_map, bg = "white")






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
# Merge with hotspot areas (all hotspots, so those without CCE show 0)
result <- dplyr::left_join(unique(as.data.frame(hotspots)[, c("NAME", "hotspot_area_km2")]),
                           intersected_by_hotspot,
                           by = "NAME")
result$x[is.na(result$x)] <- 0
result$percent_hotspot_covered <- (result$x / result$hotspot_area_km2) * 100
result$percent_of_CCE <- (result$x / sum(th_we_bin_area)) * 100
mean(result$percent_hotspot_covered[result$x > 0])
# Baseline: share of tropical America vs. share of the CCE inside hotspots
hotspots_diss <- terra::aggregate(hotspots)
hotspot_baseline <- data.frame(
  pct_TA_in_hotspots = 100 * terra::expanse(hotspots_diss, unit = "km") /
    terra::expanse(tropical_america_diss, unit = "km"),
  pct_CCE_in_hotspots = 100 * sum(terra::expanse(terra::intersect(th_we_bin_dis, hotspots_diss), unit = "km")) /
    sum(th_we_bin_area))
hotspot_baseline$ratio <- hotspot_baseline$pct_CCE_in_hotspots / hotspot_baseline$pct_TA_in_hotspots
writexl::write_xlsx(list(per_hotspot = result, baseline = hotspot_baseline),
                    file.path(file_path3, "top90_per_hotspot.xlsx"))
rm(hotspots_diss)

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

# Numbers for the manuscript ----------------------------------------------
# Hotspots with substantial overlap (>= 10% of their area covered by CCE)
hot_cov <- result$percent_hotspot_covered[result$percent_hotspot_covered >= 10]
dir.create(here::here("outputs", "numbers"), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(data.frame(key = c(we_vs_effort$metric,
                                    "pct_TA_in_hotspots", "pct_CCE_in_hotspots", "ratio_CCE_hotspots",
                                    "pct_CCE_outside_hotspots", "min_hotspot_covered", "max_hotspot_covered",
                                    "mean_hotspot_covered",
                                    paste0("pct_hotspot_covered_", make.names(result$NAME)),
                                    "pct_CCE_protected", "pct_TA_protected",
                                    paste0(rep(c("pct_cells_TA_", "pct_CCE_TA_", "n_CCE_biome_patches_"),
                                               each = nrow(cce_eco)), cce_eco$ecoregion),
                                    "pct_negro_branco_in_CCE"),
                            value = c(we_vs_effort$value,
                                      hotspot_baseline$pct_TA_in_hotspots, hotspot_baseline$pct_CCE_in_hotspots,
                                      hotspot_baseline$ratio, 100 - hotspot_baseline$pct_CCE_in_hotspots,
                                      min(hot_cov), max(hot_cov), mean(hot_cov),
                                      result$percent_hotspot_covered,
                                      100 * sum(intersection1$intersected_area_km2) / sum(th_we_bin_area),
                                      100 * terra::expanse(wdpa_data, unit = "km") /
                                        terra::expanse(tropical_america_diss, unit = "km"),
                                      cce_eco$pct_cells_TA, cce_eco$pct_CCE_TA, cce_eco$n_patches_biome,
                                      sum(er_we_int_df$Percentage[er_we_int_df[[1]] %in% "Negro-Branco moist forests"])),
                            description = c(paste("WE vs sampling effort:", we_vs_effort$metric),
                                            "% of tropical America inside hotspots", "% of CCE area inside hotspots",
                                            "CCE share / area share of hotspots", "% of CCE area outside hotspots",
                                            "Min % of hotspot area covered by CCE (hotspots with >= 10% covered)",
                                            "Max %", "Mean %",
                                            paste("% of hotspot area covered by CCE:", result$NAME),
                                            "% of CCE area in strictly protected areas",
                                            "% of tropical America in strictly protected areas",
                                            paste(rep(c("% of tropical America cells in", "% of CCE (Fig 2) in",
                                                        "CCE patches (Fig 3) in"), each = nrow(cce_eco)),
                                                  cce_eco$ecoregion),
                                            "% of the Negro-Branco moist forests ecoregion inside the CCE")),
                 here::here("outputs", "numbers", "09_preparing_plots.csv"), row.names = FALSE)
rm(hot_cov)



