
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
file4 <- "interpolated_maps25km.tiff"
file5 <- "final_df.rds"
file6 <- "missing_trees.rds"
file7 <- "checklist_standardized.rds"
file8 <- "we_map_area_50km_null_model.rds"
file9 <- "efg_summary_table.csv"
file10 <- "hotspots_2016_1.shp"

end_efg <- readRDS(file.path(file_path1, file1))
spp_efg <- readRDS(file.path(file_path1, file2))
wee_map <- terra::rast(file.path(file_path2, file3)) 
int_map <- terra::rast(file.path(file_path2, file4))
null_model <- readRDS(file.path(file_path1, file8))
scatt <- read.csv(file.path(file_path3, file9))
my_df <- readRDS(file.path(file_path1, file5)) 
my_df1 <- my_df[, c("decimalLongitude.new", "decimalLatitude.new")]
my_df1 <- terra::vect(my_df1, 
                      geom = c("decimalLongitude.new",
                               "decimalLatitude.new"),
                      crs = "epsg:4326")
our_names <- readRDS(file.path(file_path0, file7))
a <- our_names[grepl("herb", our_names$lifeForm_aggregated),]

# Shapefiles...
america <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp"))
tropical_america <- america[america$Morrone_re %in% "Neotropical", ]
tropical_america_diss <- terra::aggregate(tropical_america, cores = 8)
# hotspots <- terra::vect(file.path("data", "raw-data", "maps", file10))
# hotspots <- terra::project(hotspots, tropical_america)
# 
# hotspots <- hotspots[hotspots$NAME %in% c("Atlantic Forest",
#                                           "Caribbean Islands",
#                                           "Cerrado",
#                                           "Chilean Winter Rainfall and Valdivian Forests",
#                                           "Madrean Pine-Oak Woodlands",
#                                           "Mesoamerica",
#                                           "Tropical Andes",
#                                           "Tumbes-Choco-Magdalena")]
# hotspots <- hotspots[hotspots$Type %in% "hotspot area", ]

# Preparing the study region map ------------------------------------------
america_neo <- america[america$Morrone_re %in% "Neotropical", ]
cores <- MetroBrewer::metro.brewer("Beijing", 
                                   n = 19,
                                   return_hex = T)

names(cores) <- america_neo$our_efg
america_neo$our_efg <- factor(america_neo$our_efg, levels = names(cores))

svg("figures/StudyArea.svg", width = 10, height = 10)
# Main plot of Neotropics
terra::plot(america_neo, 
            col = cores[america_neo$our_efg],
            # main = "Neotropical Ecossystem Functional Groups",
            xlab = "Longitude",
            ylab = "Latitude",
            cex.lab = 1.5,
            pax = list(cex.axis = 1.4))

# The rest of it
terra::plot(america[!america$Morrone_re %in% "Neotropical", ],
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
quantile_vals <- terra::global(dens, quantile, probs = seq(0, 1, 0.05), na.rm = TRUE)
breaks <- as.numeric(quantile_vals)
labels <- names(quantile_vals)

# 90th percent quantile for more records per area
# th_dens <- terra::global(log(dens), fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
svg("figures/Figure1.svg", width = 10, height = 10)

par(mfrow = c(2, 2))
terra::plot(log(dens),
            main = "Sampling effort",
            col = hcl.colors(n = 100, "RdYlBu", rev = T),
            # add = TRUE,
            cex.main = 1.75,
            cex.lab = 1.5,
            pax = list(cex.axis = 1.4),
            legend = F)

terra::plot(tropical_america_diss,
            # xlab = "Longitude",
            # ylab = "Latitude",
            # main = "Sampling effort (50 x 50 km)",
            cex.lab = 1.5,
            pax = list(cex.axis = 1.4),
            add = TRUE)
# terra::sbar(d = 1000, type = "bar", divs = 3, 
#             xy = "bottomright", lonlat = TRUE, below = "km")
# legend(x = -100,
#        y = -20,
#        labels = paste(seq(0, 100, 20)),
#        fill = breaks)

# Plotting
# Get 90th quantile for each case
th_we <- terra::global(int_map$krig.we, fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
# Do the same for obs richness and simpson
th_s.obs <- terra::global(int_map$krig.s.obs, fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]
th_simp.obs <- terra::global(int_map$krig.simp.obs, fun = quantile, probs = 0.90, na.rm = TRUE)[1,1]

# Get contours only for all maps
th_we_bin <- int_map$krig.we > th_we
th_we_bin <- terra::as.polygons(th_we_bin, dissolve = TRUE)
th_s.obs_bin <- int_map$krig.s.obs > th_s.obs
th_s.obs_bin <- terra::as.polygons(th_s.obs_bin, dissolve = TRUE)
th_simp.obs_bin <- int_map$krig.simp.obs > th_simp.obs
th_simp.obs_bin <- terra::as.polygons(th_simp.obs_bin, dissolve = TRUE)

# Species richness
terra::plot(int_map$krig.s.obs,
            #
            #
            main = "Species richness",
            cex.main = 1.75,
            cex.lab = 1.5,
            pax = list(cex.axis = 1.4),
            col = hcl.colors(n = 100, "RdYlBu", rev = T),
            legend = F)
terra::plot(th_s.obs_bin[th_s.obs_bin$krig.s.obs == 1, ],
            lwd = 3, add = T)


# Simpson diversity
terra::plot(int_map$krig.simp.obs,
            #
            #
            main = "Simpson diversity",
            cex.main = 1.75,
            cex.lab = 1.5,
            pax = list(cex.axis = 1.4),
            col = hcl.colors(n = 100, "RdYlBu", rev = T),
            legend = F)
terra::plot(th_simp.obs_bin[th_simp.obs_bin$krig.simp.obs == 1, ],
            lwd = 3, add = T)

# WE
terra::plot(int_map$krig.we, 
            #xlab = "Longitude",
            #ylab = "Latitude",
            main = "Weighted endemism",
            cex.main = 1.75,
            cex.lab = 1.5,
            pax = list(cex.axis = 1.4),
            col = hcl.colors(n = 100, "RdYlBu", rev = T), 
            legend = F)
terra::plot(th_we_bin[th_we_bin$krig.we == 1, ],
            lwd = 3, add = T)

dev.off()



# Top 90% WE values inside each biome (area) ------------------------------
intersected <- terra::intersect(th_we_bin[th_we_bin$krig.we == 1, ], 
                                tropical_america)
# Area
intersected$top90_area_m2 <- terra::expanse(intersected)
intersected$top90_area_km2 <- intersected$top90_area_m2 / 1e6
# Aggregate the results by our_efg
result <- terra::aggregate(top90_area_km2 ~ our_efg, data = as.data.frame(intersected), FUN = sum)

# Get the info into the summary_table
scatt <- dplyr::left_join(scatt, result, by = c("EFG" = "our_efg"))
scatt$prop_top90_area_km2 <- 100 * (scatt$top90_area_km2 / scatt$EFG_area_km)
write.csv(scatt, file.path(file_path3, file9))



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


