# Zenodo deposit ----------------------------------------------------------
# Copies, renames and exports the maps, records and datasets that support the
# paper into outputs/zenodo. Run after 10_preparing_checklist.R (which saves
# checklist_records.rds) and 09_preparing_plots.R. Nothing is deleted: files
# with the same name are overwritten

out_path <- here::here("outputs", "zenodo")
maps_path <- here::here("data", "derived-data", "maps")
for (d in c("01_maps", "02_records", "03_datasets")) {
  dir.create(file.path(out_path, d), showWarnings = FALSE, recursive = TRUE)
}

# 1. Biome map: ecoregions (Dinerstein et al. 2017) re-coded into the 21 EFGs
biomes_map <- terra::vect(file.path(maps_path, "NTAmericas_efgs.shp"))
terra::writeVector(biomes_map, file.path(out_path, "01_maps", "biomes_tropical_america.gpkg"),
                   overwrite = TRUE)

# 2. GADM administrative units classified by the share of their area in a
# single biome (>= 90%, 75-90%, < 75%; Table S2)
file.copy(file.path(maps_path, "latamMap_classified.gpkg"),
          file.path(out_path, "01_maps", "admin_units_biome_class.gpkg"), overwrite = TRUE)

# 3. Weighted endemism (WE) and richness at 0.5 degree (~50 km)
int_map <- terra::rast(file.path(maps_path, "interpolated_maps_50km.tiff"))
cat("Layers in interpolated_maps_50km.tiff:", names(int_map), "\n")
# Kriged maps (Figure 2, Figure S5)
terra::writeRaster(int_map[[c("krig.we", "krig.s.obs")]],
                   file.path(out_path, "01_maps", "kriged_WE_richness_50km_tropical_america.tif"),
                   overwrite = TRUE)
# Observed values before kriging: WE, richness, number of records and the null
# model (Figure S7). Only the layers present in the file are kept
obs_layers <- intersect(c("we", "S_obs", "N", "slope", "we_null_out_continuous",
                          "we_null_P_above", "we_null_out_above_below"), names(int_map))
terra::writeRaster(int_map[[obs_layers]],
                   file.path(out_path, "01_maps", "observed_WE_50km_tropical_america.tif"),
                   overwrite = TRUE)

# Kriged maps of each biome (Figure 3, Figure S6)
biome_files <- list.files(maps_path, pattern = "^interpolated_maps_biomes_50km_.*\\.tiff$",
                          full.names = TRUE)
biome_names <- sub("^interpolated_maps_biomes_50km_(.*)\\.tiff$", "\\1", basename(biome_files))
# "Tropical.subtropical.lowland.rainforests" -> "lowland_rainforests"
biome_short <- gsub("\\.", "_", sub("^Tropical\\.subtropical\\.", "", biome_names))
biome_short <- tolower(biome_short)
int_map_biomes <- lapply(biome_files, terra::rast)
names(int_map_biomes) <- biome_short
for (i in biome_short) {
  terra::writeRaster(int_map_biomes[[i]][[c("krig.we", "krig.s.obs")]],
                     file.path(out_path, "01_maps", paste0("kriged_WE_richness_50km_", i, ".tif")),
                     overwrite = TRUE)
}

# 4. Critical centers of endemism (CCE): cells above the 90th percentile of the
# kriged WE, as in Figures 2 and 3 (09_preparing_plots.R)
cce_file <- file.path(out_path, "01_maps", "CCE_tropical_america_and_biomes.gpkg")
th_we <- terra::global(int_map$krig.we, fun = quantile, probs = 0.90, na.rm = TRUE)[1, 1]
cce_ta <- terra::as.polygons(int_map$krig.we > th_we, dissolve = TRUE)
cce_ta <- cce_ta[cce_ta$krig.we == 1, ]
terra::writeVector(cce_ta, cce_file, layer = "tropical_america", overwrite = TRUE)
for (i in biome_short) {
  r <- int_map_biomes[[i]][["krig.we"]]
  cce_b <- terra::as.polygons(r >= terra::global(r, quantile, probs = 0.90, na.rm = TRUE)[1, 1],
                              dissolve = TRUE)
  cce_b <- cce_b[cce_b$krig.we == 1, ]
  terra::writeVector(cce_b, cce_file, layer = i, insert = TRUE, overwrite = TRUE)
}

# 5. Records of the checklist dataset (spatial dataset + extra records)
records <- readRDS(here::here("data", "derived-data", "rds", "checklist_records.rds"))
cat("Checklist records:", nrow(records), "\n")
print(table(records$dataset))
data.table::fwrite(records, file.path(out_path, "02_records", "checklist_dataset_records.csv.gz"))

# 6. Datasets S1-S6, copied from outputs/tables to the Zenodo folder and to the
# PNAS submission folder (same files submitted as SI)
ds_files <- here::here("outputs", "tables", c("richness_by_nt.xlsx", "endemics_by_nt.xlsx",
                                              "probable_occurrence.xlsx", "checklist_richness_biome.xlsx",
                                              "checklist_endemics_biome.xlsx", "er_area_inside_cce.xlsx"))
file.copy(ds_files, file.path(out_path, "03_datasets", paste0("Dataset_S", 1:6, ".xlsx")), overwrite = TRUE)
file.copy(ds_files, here::here("outputs", "PNAS_manuscript", paste0("Dataset_S", 1:6, ".xlsx")),
          overwrite = TRUE)

# 7. README
writeLines(c(
  "Data for: A biome lens is critical to understanding tree endemism across tropical America",
  "",
  "All spatial files use WGS84 (EPSG:4326). Rasters have 0.5 degree (~50 km) cells.",
  "",
  "01_maps/",
  "  biomes_tropical_america.gpkg: terrestrial ecoregions (Dinerstein et al. 2017) re-coded into",
  "    the 21 biomes (IUCN Global Ecosystem Typology EFGs) of tropical America (Morrone et al. 2022).",
  "  admin_units_biome_class.gpkg: GADM administrative units classified by the share of their area",
  "    within a single biome (>= 90%: richness and endemism; 75-90%: richness only; < 75%: transition).",
  "  kriged_WE_richness_50km_tropical_america.tif: ordinary kriging of weighted endemism (krig.we)",
  "    and richness (krig.s.obs) of the tree species endemic to tropical America (Figure 2, Figure S5).",
  "  kriged_WE_richness_50km_<biome>.tif: the same for the species endemic to each of the four",
  "    biomes with most endemics (Figure 3, Figure S6).",
  paste0("  observed_WE_50km_tropical_america.tif: values before kriging (layers: ",
         paste(obs_layers, collapse = ", "), ")."),
  "    we = weighted endemism; S_obs = observed richness; N = number of records; slope = slope of the",
  "    species accumulation curve; we_null_* = null-model deviation, P-value and outlier class (Figure S7).",
  "  CCE_tropical_america_and_biomes.gpkg: critical centers of endemism (cells above the 90th",
  "    percentile of kriged WE); one layer for tropical America and one per biome.",
  "",
  "02_records/",
  "  checklist_dataset_records.csv.gz: occurrence records of the checklist dataset. Column 'dataset'",
  "    is 'spatial' for records of the spatial dataset (WE and CCE analyses) and 'extra' for records",
  "    assigned to tropical America through their administrative unit only.",
  "    Records come from GBIF (download DOI: https://doi.org/10.15468/DL.E39T7B), JABOT, REFLORA and speciesLink; cite the original",
  "    sources and respect their licenses when reusing them.",
  "",
  "03_datasets/",
  "  Dataset_S1-S6.xlsx: the Datasets S1-S6 of the paper (see their legends in the SI Appendix).",
  "",
  "Code: https://github.com/synTreeSys/Taxonomy and https://github.com/ggrittz/Endemism"
), file.path(out_path, "README.txt"))

rm(out_path, maps_path, d, biomes_map, int_map, obs_layers, biome_files, biome_names, biome_short,
   int_map_biomes, i, cce_file, th_we, cce_ta, r, cce_b, records, ds_files)
