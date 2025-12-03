
# Plots and figures and stuff ---------------------------------------------

# Load necessary data
file_path0 <- here::here("data", "derived-data")
file_path1 <- here::here("data", "derived-data", "rds")
file_path2 <- here::here("data", "derived-data", "maps")
file_path3 <- here::here("outputs", "tables")
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
file9 <- "efg_summary_table.csv"
# file10 <- "hotspots_2016_1.shp"

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
tropical_america_diss <- terra::aggregate(tropical_america, cores = 8)
tropical_america_diss <- terra::fillHoles(tropical_america_diss)

# Whole
america <- terra::aggregate(america, cores = 8)
america <- terra::fillHoles(america)

# Preparing the study region map ------------------------------------------
cores <- MetroBrewer::metro.brewer("Beijing", 
                                   n = 19,
                                   return_hex = T)

names(cores) <- tropical_america$our_efg
tropical_america$our_efg <- factor(tropical_america$our_efg, levels = names(cores))

svg("figures/FigureS1.svg", width = 10, height = 10)
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
th_we_bin_dis <- th_we_bin_dis[th_we_bin_dis$area_km2 > 50, ]
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
my_lay <- matrix(c(1, 2, 3, 7, 1, 4, 5, 7, 6, 6, 6, 6),
                 nrow = 3, ncol = 4, byrow = TRUE)
{
svg("figures/Figure1.svg", width = 33, height = 30)

layout(my_lay, 
       widths = c(0.05, 0.4375, 0.4375, 0.075),
       heights = c(0.475, 0.475, 0.05))
layout.show(7)

# Shared y-axis
par(mar = c(0, 0, 0, 0))
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
mtext("Latitude", side = 4, line = -8, cex = 4.5)

# Sampling effort
par(mar = c(0, 0, 0, 0))
terra::plot(log(dens),
            main = "(a) Sampling effort",
            col = hcl.colors(n = 100, "RdYlBu", rev = T),
            cex.main = 6,
            pax = list(cex.axis = 5, las = 1, xaxt = "n", mgp = c(3, 2.5, 0)),
            legend = F,
            axes = T)
terra::plot(america, add = TRUE)

# Species richness
par(mar = c(0, 0, 0, 0))
terra::plot(int_map$krig.s.obs,
            main = "(b) Species richness",
            cex.main = 6,
            pax = list(cex.axis = 5, las = 1, xaxt = "n", yaxt = "n"),
            col = hcl.colors(n = 100, "RdYlBu", rev = T),
            legend = F,
            axes = T)
terra::plot(th_s.obs_bin_dis, lwd = 5, add = T)
terra::plot(america, add = TRUE)

# Simpson diversity
par(mar = c(0, 0, 0, 0))
terra::plot(int_map$krig.simp.obs,
            main = "(c) Simpson diversity",
            cex.main = 6,
            pax = list(cex.axis = 5, las = 1, mgp = c(3, 2.5, 0)),
            col = hcl.colors(n = 100, "RdYlBu", rev = T),
            legend = F,
            axes = T)
terra::plot(th_simp.obs_bin_dis, lwd = 5, add = T)
terra::plot(america, add = TRUE)

# Weighted endemism
par(mar = c(0, 0, 0, 0))
terra::plot(int_map$krig.we, 
            main = "(d) Weighted endemism",
            cex.main = 6,
            pax = list(cex.axis = 5, las = 1, yaxt = "n", mgp = c(3, 2.5, 0)),
            col = hcl.colors(n = 100, "RdYlBu", rev = T), 
            legend = F,
            axes = T)
terra::plot(th_we_bin_dis, lwd = 5, add = T)
terra::plot(america, add = TRUE)
# box()

# Shared x-axis
par(mar = c(0, 0, 0, 0))
plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
mtext("Longitude", side = 3, line = -3, cex = 4)

# Legend bar
par(mar = c(0, 9.5, 0, 0))
plot(1, type = "n", xlim = c(0, 1), ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "")
colramp <- hcl.colors(n = 100, "RdYlBu", rev = T)
rect(0.2, 0:99, 0.8, 1:100, col = colramp, border = NA)
axis(2, at = c(0, 50, 100), labels = c("Low", "Medium", "High"), las = 1, cex.axis = 4.5, line = -0.5)
# text(0.5, -5, "Value", xpd = TRUE, cex = 1.2)
dev.off()
}



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
write.csv(intersected_df, file.path(file_path3, file9))



# Making one plot for each biome here -------------------------------------
# Fix the layout matrix for 5 plots
# my_layout <- matrix(c(1, 2, 3, 2, 4, 5),  # 0 for empty cell
#                     nrow = 3, ncol = 2, byrow = TRUE)
# layout(my_layout, 
#        widths = c(0.5, 0.5),
#        heights = c(0.333, 0.333, 0.333))
# layout.show(5)  
# 
# my_order <- c(3, 2, 1, 1, 2)
my_order_biome_names <- biome_names
# my_order_biome_names <- my_order_biome_names[my_order]

for(i in seq_along(my_order_biome_names)){
  svg(paste0(here::here("figures"), "/", "FigureS3_", 
             my_order_biome_names[i], ".svg"), width = 10, height = 13)
  biome_name <- my_order_biome_names[i]
  
  th_we <- terra::global(int_map_biomes[[biome_name]], fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
  th_we_bin <- int_map_biomes[[biome_name]] >= th_we
  th_we_bin <- terra::as.polygons(th_we_bin, dissolve = TRUE)
  th_we_bin <- th_we_bin[th_we_bin$lyr.1 == 1, ]
  
  # Disaggregate to measure polygon sizes
  # th_we_bin_dis <- terra::disagg(th_we_bin)
  # th_we_bin_area <- terra::expanse(th_we_bin_dis, unit = "km")
  par(mar = c(2, 3, 2, 2))
  terra::plot(int_map_biomes[[biome_name]], 
              col = hcl.colors(n = 100, "RdYlBu", rev = T),
              main = gsub("\\.", " ", biome_name),  # Use biome_name directly
              xlab = "Longitude",
              ylab = "Latitude",
              cex.lab = 1.5,
              pax = list(cex.axis = 1.4))
  terra::plot(th_we_bin, lwd = 3, add = T)
  terra::plot(tropical_america_diss, add = TRUE)
  dev.off()
}



##### POSSIBLY NOT USING ANYTHING BELOW HERE #####


# Network map -------------------------------------------------------------
library(igraph)
library(tidyverse)
library(RColorBrewer)

# add codes to names

# Use only these 
end_efg1 <- end_efg[names(end_efg) %in% names(spp_efg)]
# Need to add the ones without endemics too
end_efg1$`Seasonally dry temperate heaths and shrublands` <- character(0)
end_efg1$`Temperate alpine grasslands and shrublands` <- character(0)
end_efg1$`Cool deserts and semi-deserts` <- character(0)
end_efg1 <- end_efg1[sort(names(end_efg1))]

# Total species per biome
total_species <- sapply(spp_efg, length)

# For posterior analyses, use only top 10 richness
# Only top 10 in richness
top_10 <- names(sort(total_species, decreasing = TRUE))[1:10]
spp_efg10 <- spp_efg[top_10]
end_efg10 <- end_efg1[top_10]
# Endemic species per biome
endemic_counts <- sapply(end_efg10, length)
# Endemism level
endemic_percent <- endemic_counts / sort(total_species, decreasing = TRUE)[1:10] * 100

# Shared species matrix
shared_matrix <- matrix(0, nrow = length(spp_efg10), ncol = length(spp_efg10))
rownames(shared_matrix) <- names(spp_efg10)
colnames(shared_matrix) <- names(spp_efg10)

for (i in seq_along(names(spp_efg10))) {
  for (j in seq_along(names(spp_efg10))) {
    shared_matrix[i,j] <- length(intersect(spp_efg10[[i]], spp_efg10[[j]]))
  }
}

# Network graph
g <- graph_from_adjacency_matrix(shared_matrix,
                                 mode = "undirected",
                                 weighted = TRUE,
                                 diag = FALSE)

# Vertices == number of endemics
V(g)$size <- sqrt(endemic_counts) + 5 # transformed for visualization
V(g)$color <- colorRampPalette(brewer.pal(9, "Greens"))(100)[cut(endemic_percent, breaks = 100)]
# Edges == shared richness
min_width <- 5
max_width <- 30
E(g)$width <- scales::rescale(E(g)$weight, to = c(min_width, max_width))
# Color edges based on shared richness too
edge_colors <- colorRampPalette(c("gray80", "darkgreen"))(100)
normalized_weights <- round(scales::rescale(E(g)$weight, to = c(1, 100)))
E(g)$color <- edge_colors[normalized_weights]

par(family = "sans")
plot(g,
     layout = layout_in_circle(g),
     vertex.label.cex = 1,
     vertex.label.color = "black",
     edge.curved = 0.2,
     vertex.frame.color = NA,
     margin = c(0,0,0,0))



##### EFG SUMMARY SCATTERPLOT #####
library(ggplot2)
library(ggpmisc)
new_row <- data.frame(X = NA, 
                      EFG = "Oceanic cool temperate rainforests",
                      Richness = NA,
                      Endemics = NA,
                      Endemism_level = NA,
                      EFG_area_km = NA)
# Just to fix colors...
scatt <- rbind(scatt, new_row)
scatt <- scatt[order(scatt$EFG), ]
cores <- MetroBrewer::metro.brewer("Beijing", n = 19)
names(cores) <- scatt$EFG
scatt$EFG <- factor(scatt$EFG, levels = names(cores))
scatt1 <- scatt[!is.na(scatt$Richness), ]
scatt1[is.na(scatt1)] <- 0
scatt2 <- scatt1[scatt1$Endemics > 0, ]


ggplot(scatt2, aes(x = Richness,
                   y = Endemics,
                   color = EFG)) +
  geom_point(aes(size = Endemism_level), alpha = 0.7) + 
  geom_smooth(method = "lm", 
              se = TRUE,
              color = "black",
              linewidth = 0.5,
              aes(group = 1),
              show.legend = FALSE) +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        aes(label = paste(after_stat(rr.label), 
                                          sep = "~~~")),
                        label.x = "right",
                        label.y = "top",
                        size = 4,
                        color = "black") +
  scale_x_log10() +
  scale_y_log10() +
  scale_size_continuous(range = c(3, 15)) +
  scale_color_manual(values = cores) +
  labs(x = "\nSpecies richness (log10)",
       y = "Endemic species (log10)\n",
       color = "Biome",
       size = "Endemism level (%)") +
  theme_minimal() +
  theme(legend.position = "right",
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 13), 
        legend.title = element_text(size = 13),
        legend.text = element_text(size = 13))

ggsave("figures/FigureS2.svg", device = "svg",
       height = 7, width = 12)


