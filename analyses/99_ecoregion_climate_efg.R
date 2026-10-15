# Climate of ecoregions and calibration of EFG thresholds ------------------

# Climatic water deficit (CWD), tmin and tmax per ecoregion, from TerraClimate
# 4 km annual rasters (Abatzoglou et al. 2018), and the CWD range of each EFG
# among ecoregions where our_efg and BIOM_NEW agree. Used to separate
# T1.2 (Tropical-subtropical dry forests and thickets) from T5.2 (Thorny
# deserts and semi-deserts): CWD < ~950 mm -> T1.2, > ~1200 mm -> T5.2,
# overlap decided by physiognomy (Keith et al. 2020, p. 40 and 63).

# Paths
eco_path <- here::here("data", "raw-data", "maps", "Ecoregions2017.shp")
class_path <- here::here("..", "Endemism", "data", "derived-data", "maps",
                         "final_classification.xlsx")
clim_path <- here::here("..", "Assessments", "data", "raw-data", "climatic-variables", "annual_4km")

# Classification table
eco_class <- readxl::read_xlsx(class_path)
eco_class$to_merge <- plantR::rmLatin(eco_class$ECO_NAME)
print(unique(eco_class$ECO_NAME[duplicated(eco_class$to_merge) &
                                  !duplicated(eco_class[, c("to_merge", "our_efg")])]))
eco_class <- eco_class[!duplicated(eco_class$to_merge), ]

# Ecoregions 2017
er <- terra::vect(eco_path)
er <- er[, "ECO_NAME"]
er$to_merge <- plantR::rmLatin(er$ECO_NAME)
er <- er[er$to_merge %in% eco_class$to_merge, ]
er <- terra::buffer(er, 0)
er <- terra::aggregate(er, by = "to_merge", dissolve = TRUE, count = FALSE)

# CWD mean 1991-2020; tmin and tmax mean of 2000, 2010 and 2020
cwd <- terra::app(terra::rast(file.path(clim_path, paste0("def_", 1991:2020, ".tif"))),
                  fun = mean, na.rm = TRUE)
tmin <- terra::app(terra::rast(file.path(clim_path, paste0("tmin_", c(2000, 2010, 2020), ".tif"))),
                   fun = mean, na.rm = TRUE)
tmax <- terra::app(terra::rast(file.path(clim_path, paste0("tmax_", c(2000, 2010, 2020), ".tif"))),
                   fun = mean, na.rm = TRUE)
clim <- c(cwd, tmin, tmax)
names(clim) <- c("cwd", "tmin", "tmax")

# All cells touched by each ecoregion
clim_vals <- terra::extract(clim, er, touches = TRUE)
clim_eco <- dplyr::summarise(dplyr::group_by(clim_vals, ID),
                             cells = sum(!is.na(cwd)),
                             cwd_p10 = stats::quantile(cwd, 0.10, na.rm = TRUE),
                             cwd_med = stats::median(cwd, na.rm = TRUE),
                             cwd_p90 = stats::quantile(cwd, 0.90, na.rm = TRUE),
                             tmin_p10 = stats::quantile(tmin, 0.10, na.rm = TRUE),
                             tmin_med = stats::median(tmin, na.rm = TRUE),
                             tmax_med = stats::median(tmax, na.rm = TRUE))

# Area and latitude range per ecoregion
bbox <- t(vapply(seq_len(nrow(er)), function(i) as.vector(terra::ext(er[i, ])), numeric(4)))
eco_clim <- data.frame(to_merge = er$to_merge,
                       area_km2 = round(terra::expanse(er, unit = "km")),
                       lat_min = round(bbox[, 3], 1),
                       lat_max = round(bbox[, 4], 1),
                       clim_eco[match(seq_len(nrow(er)), clim_eco$ID), -1])
eco_clim <- dplyr::left_join(eco_class[, c("to_merge", "ECO_NAME", "BIOM_NEW", "our_efg")],
                             eco_clim, by = "to_merge")
eco_clim <- eco_clim[, -which(names(eco_clim) == "to_merge")]
eco_clim[, c("cwd_p10", "cwd_med", "cwd_p90", "tmin_p10", "tmin_med", "tmax_med")] <-
  round(eco_clim[, c("cwd_p10", "cwd_med", "cwd_p90", "tmin_p10", "tmin_med", "tmax_med")], 1)

# Calibration: CWD range of each EFG where both classifications agree
consensus <- eco_clim[!is.na(eco_clim$BIOM_NEW) & eco_clim$BIOM_NEW == eco_clim$our_efg &
                        !is.na(eco_clim$cwd_med), ]
efg_cwd <- dplyr::summarise(dplyr::group_by(consensus, our_efg),
                            n = dplyr::n(),
                            cwd_min = min(cwd_med),
                            cwd_median = stats::median(cwd_med),
                            cwd_max = max(cwd_med),
                            tmin_median = stats::median(tmin_med))
efg_cwd <- efg_cwd[order(efg_cwd$cwd_median), ]
print(efg_cwd, n = Inf)

# Overlap between T1.2 and T5.2: from the driest-CWD T5.2 to the wettest-CWD T1.2
dry_forest <- consensus$cwd_med[consensus$our_efg == "Tropical-subtropical dry forests and thickets"]
thorny <- consensus$cwd_med[consensus$our_efg == "Thorny deserts and semi-deserts"]
print(sort(dry_forest))
print(sort(thorny))
print(c(overlap_from = min(thorny), overlap_to = max(dry_forest)))

# Rule applied to every ecoregion now in T1.2 or T5.2 (thresholds rounded from the overlap)
eco_clim$cwd_rule <- ifelse(!eco_clim$our_efg %in% c("Tropical-subtropical dry forests and thickets",
                                                     "Thorny deserts and semi-deserts"), NA,
                     ifelse(eco_clim$cwd_med < 950, "T1.2",
                     ifelse(eco_clim$cwd_med > 1200, "T5.2", "overlap: physiognomy decides")))

# Save
writexl::write_xlsx(list(ecoregions = eco_clim, efg_cwd = efg_cwd),
                    here::here("outputs", "tables", "ecoregions_terraclimate_efg.xlsx"))

rm(eco_path, class_path, clim_path, eco_class, er, cwd, tmin, tmax, clim, clim_vals,
   clim_eco, bbox, consensus, dry_forest, thorny)
