
# Measuring endemism levels -----------------------------------------------

##### Read data #####

# Loading our classified latamMap
file_path <- here::here("data", "derived-data", "maps")
file_path1 <- here::here("data", "derived-data", "Cleaned_occs")
file_path2 <- here::here("data", "derived-data", "Endemic_species")
file_path3 <- here::here("data", "derived-data", "rds")
# Start from an empty folder so no old family files survive a new run
unlink(file_path2, recursive = TRUE)
dir.create(file_path2)
file_name <- "latamMap_classified.gpkg"
latam_gaz <- terra::vect(file.path(file_path, file_name))

# Load equivalence table for administrative levels that exist in gazetteer
# This is going to be used for keeping records at X, Y, Z adm level
# options(gargle_oauth_cache = ".secrets")
# googlesheets4::gs4_auth() # is this needed?
# googlesheets4::gs4_auth(cache = ".secrets",
#                         email = "ggrittz@usp.br",
#                         scopes = "https://www.googleapis.com/auth/spreadsheets.readonly")
# Download from Google Drive
# link <- "https://docs.google.com/spreadsheets/d/"
# link1 <- "1V42RsVpUQuiHbv-U6U_t--8mCAr7UOjQLpQ9Awq6PRI/edit?gid=0#gid=0"
# eq_table <- as.data.frame(googlesheets4::read_sheet(paste0(link, link1), range = "A:G"))
# eq_table <- eq_table[, c("country", "latamMap_GADM")]

# Fast preparing latam_gaz complete loc.correct1 (NAME_all) in the same way
# it is from loc.correct
latam_gaz <- terra::as.data.frame(latam_gaz, geom = "WKT")
latam_gaz$loc_gaz <- apply(latam_gaz[, 1:5], 1, function(x) {
  non_na <- x[!is.na(x)]
  paste(non_na, collapse = "_")
})

##### Dealing with states too big...validate only if small #####
latamMap <- plantR::latamMap
latamMap <- dplyr::bind_rows(latamMap)

# Convert to terra
latamMap <- terra::vect(latamMap)
# terra::plot(latamMap[latamMap$NAME_1 %in% "santa catarina", ])
# Aggregate everything into country and state-level
latamMap_agg_country <- terra::aggregate(x = latamMap, by = "NAME_0", 
                                         cores = 4, count = FALSE)
latamMap_agg_state <- terra::aggregate(x = latamMap, by = c("NAME_0", "NAME_1"), 
                                 cores = 4, count = FALSE)
# Dealing with holes after agg
latamMap_agg_country <- terra::fillHoles(latamMap_agg_country)
latamMap_agg_state <- terra::fillHoles(latamMap_agg_state)
# Measure the area of each polygon
latamMap_agg_country$area_km <- terra::expanse(latamMap_agg_country, unit = "km")
latamMap_agg_state$area_km <- terra::expanse(latamMap_agg_state, unit = "km")
# Matching column
latamMap_agg_country$name <- paste0(latamMap_agg_country$NAME_0)
latamMap_agg_state$name <- paste(latamMap_agg_state$NAME_0, latamMap_agg_state$NAME_1, sep = "_")
# Do not use a country/state-validated occurrence if its area is above: 12,500 km²
# around 13 % of all latin American states are below 12,500 km², for example
# This value was decided based on the 50 x 50 km grid cells
# At country
latamMap_agg_country$bad_reso <- latamMap_agg_country$area_km >= 12500
# At state
latamMap_agg_state$bad_reso <- latamMap_agg_state$area_km >= 12500
# Df
latamMap_agg_country <- terra::as.data.frame(latamMap_agg_country)
latamMap_agg_state <- terra::as.data.frame(latamMap_agg_state)
# Join
latamMap_agg_adm <- rbind.data.frame(latamMap_agg_country[, c("name", "bad_reso")],
                                     latamMap_agg_state[, c("name", "bad_reso")])
saveRDS(latamMap_agg_adm, here::here("data", "derived-data", "rds", "latamMap_agg_adm.rds"))

# Back to vect
# latam_gaz <- terra::vect(latam_gaz, geom = "geometry", crs = "epsg:4326")

# Morrone 2022 (map dissolved by Realms)
tropical_america <- here::here("data", "derived-data", "maps", "NTAmericas_dissolved.shp")
tropical_america <- terra::vect(tropical_america)
tropical_america <- terra::project(tropical_america, "epsg:4326")
tropical_america <- tropical_america[tropical_america$Morrone_re %in% "Neotropical", ]

# Morrone 2022 (map dissolved by EFGs)
efgs <- here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp")
efgs <- terra::vect(efgs)
efgs <- terra::project(efgs, "epsg:4326")
efgs <- efgs[efgs$Morrone_re %in% "Neotropical", ]

# Gazetteer points
gaz_efg <- terra::vect(here::here("data", "derived-data", "maps", "gaz_intersected_efg.shp"))
gaz_efg <- terra::as.data.frame(gaz_efg, geom = NULL)
gaz_nt <- terra::vect(here::here("data", "derived-data", "maps", "gaz_intersected_nt.shp"))
gaz_nt <- terra::as.data.frame(gaz_nt, geom = NULL)
names(gaz_nt)[which(names(gaz_nt) == "Morrone_re")] <- "is_nt"
gaz_nt$is_nt <- ifelse(gaz_nt$is_nt %in% "Neotropical", TRUE, FALSE)

# Loading family names
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))
# family_names <- family_names[5:311]

# Loading species list
our_names <- readRDS(here::here("data", "derived-data", "checklist_species_level.rds"))
our_names <- our_names[!duplicated(our_names$our_accepted_scientificName), ]

# Vouchers from FFB
vouchers <- readRDS(file.path(file_path3, "ffb_vouchers.rds"))
# Voucher + binomial keys (FFB uses these vouchers for species distributions)
ffb_keys <- paste(vouchers$voucher_these,
                  sub("^([^ ]+ +[^ ]+).*$", "\\1", vouchers$taxonName),
                  sep = "_")


# Infraspecific accepted names are moved up to their species in the Taxonomy
# project (12_species_level_checklist.R), so no taxonomy fix is needed here

# Store each filtering step
all_filtering_summaries <- list()
n_country_level <- c()

# Do it by family
for(i in family_names){
  
  cat("Calculating endemism levels for", i, "\n")
  rds_path <- file.path(file_path1, i, paste0(i, ".rds"))

  if(!file.exists(rds_path)){
    cat("Input file not found, skipping \n")
    next
  }

  family_data <- readRDS(rds_path)
  
  
  # Initialize filter counts for this family
  filter_counts <- data.frame(
    family = i,
    original_n = NA,
    first_tax_filter = NA,
    second_geo_filter = NA,
    third_loc_filter = NA,
    fourth_bad_reso_filter = NA,
    fifth_coord_filter = NA,
    sixth_cult_filter = NA,
    seventh_outlier_filter = NA,
    stringsAsFactors = FALSE
  )
  
  # Records matching an FFB voucher of the same species become "high"
  occ_keys <- paste(family_data$recordedBy.new, family_data$recordNumber.new,
                    sub("^([^ ]+ +[^ ]+).*$", "\\1", family_data$scientificName),
                    sep = "_")
  family_data$tax.check1[occ_keys %in% ffb_keys] <- "high"

  # Keep generalists too (14/10/2025 - I think I am removing them...30/10 removed)
  filter_counts$original_n <- nrow(family_data)
  # Non-duplicate records validated only at the country level (no "_" in loc.correct1)
  n_country_level[i] <- sum(!is.na(family_data$loc.correct1) & !grepl("_", family_data$loc.correct1))
  family_data <- family_data[family_data$tax.check1 %in% "high", ]
  filter_counts$first_tax_filter <- nrow(family_data)
  
  # Get only "trees" lifeForm
  family_data <- dplyr::left_join(family_data, 
                                  our_names[, c("our_accepted_scientificName",
                                                "lifeForm_aggregated")],
                                  by = c("scientificName" = "our_accepted_scientificName"))
  family_data <- family_data[grepl("tree", family_data$lifeForm_aggregated), ]
  
  # Geo check: keep only ok_, shore
  family_data <- family_data[grepl("^ok_|shore", 
                                   family_data$geo.check1), ]
  filter_counts$second_geo_filter <- nrow(family_data)
  
  # Lock check: keep only ok_ or check_local.2municip.
  family_data <- family_data[grepl("^ok_|^check_local.2municip.$",
                                   family_data$loc.check1), ]
  filter_counts$third_loc_filter <- nrow(family_data)
  
  # Keep registers at state-level if the state is smaller than 12,500 km²
  family_data <- dplyr::left_join(family_data, latamMap_agg_adm[, c("name", "bad_reso")],
                                  by = c("loc.correct1" = "name"))
  
  # Keep only NA and FALSE on bad_reso
  family_data <- family_data[family_data$bad_reso %in% c(FALSE, NA), ]
  filter_counts$fourth_bad_reso_filter <- nrow(family_data)
  
  # Coord check: keep only records with coordinates
  family_data <- family_data[grepl("coord_gazet|coord_original", family_data$origin.coord1), ]
  filter_counts$fifth_coord_filter <- nrow(family_data)
  
  # Cult check: keep only non-cultivated registers
  family_data <- family_data[!family_data$cult.check %in% c("cultivated", "prob_cultivated"), ]
  filter_counts$sixth_cult_filter <- nrow(family_data)
  
  # Outliers and distribution check: remove bad_dist and out.check is TRUE
  df <- 2
  alpha <- 0.025
  critVal <- qchisq(p = 1 - alpha, df = df)
  family_data$chisq.out.robust <- as.numeric(family_data$maha.robust) > critVal
  # Removing records both considered as spatial outlier and outside of its
  # known distribution
  family_data <- family_data[family_data$chisq.out.robust %in% FALSE |
                               !family_data$dist.check %in% 
                               c("bad_dist", "introduced"), ]
  # Cases where there is no possible indication of outlier
  family_data <- family_data[!(is.na(family_data$out.check) & 
                                 family_data$dist.check %in% "bad_dist"), ]
  filter_counts$seventh_outlier_filter <- nrow(family_data)
  
  # Add this family's summary to the list
  all_filtering_summaries[[i]] <- filter_counts
  
  # Continue with the rest of the code (coordinate conversion, map intersections, etc.)
  # Convert coordinate character columns to numeric
  family_data$decimalLatitude.new1 <- as.numeric(family_data$decimalLatitude.new1)
  family_data$decimalLongitude.new1 <- as.numeric(family_data$decimalLongitude.new1)
  
  # Cross-check the register with original coordinates against the maps
  with_coord_orig <- which(family_data$origin.coord1 %in% "coord_original")
  family_data_coord_orig <- terra::vect(family_data[with_coord_orig, ],
                                        geom = c("decimalLongitude.new1",
                                                 "decimalLatitude.new1"),
                                        crs = "epsg:4326")
  
  family_data_coord_orig$is_nt <- terra::is.related(x = family_data_coord_orig,
                                                    y = tropical_america,
                                                    "intersects")
  family_data_coord_orig_efgs <- terra::intersect(x = family_data_coord_orig,
                                                  y = efgs)
  
  if(all(family_data_coord_orig$is_nt %in% FALSE)){
    next
  }
  if(dim(family_data_coord_orig_efgs)[1] == 0){
    next
  }
  
  family_data_coord_orig_df <- terra::as.data.frame(family_data_coord_orig, geom = "XY")
  family_data_coord_orig_efgs <- terra::as.data.frame(family_data_coord_orig_efgs, geom = "XY")
  
  dup_ids <- family_data_coord_orig_efgs$ids[duplicated(family_data_coord_orig_efgs$ids) | 
                                               duplicated(family_data_coord_orig_efgs$ids, 
                                                          fromLast = TRUE)]
  family_data_coord_orig_efgs <- family_data_coord_orig_efgs[!family_data_coord_orig_efgs$ids %in% dup_ids, ]
  
  result_df_orig <- dplyr::left_join(x = family_data_coord_orig_df,
                                     y = family_data_coord_orig_efgs[, c("ids", "our_efg")],
                                     by = "ids")
  
  with_coord_gazet <- which(family_data$origin.coord1 %in% "coord_gazet")
  result_df_gazet <- dplyr::left_join(x = family_data[with_coord_gazet, ],
                                      y = gaz_efg[, c("loc.correc", "our_efg")],
                                      by = c("loc.correct1" = "loc.correc"))
  result_df_gazet <- result_df_gazet[!duplicated(result_df_gazet$ids), ]
  result_df_gazet <- dplyr::left_join(result_df_gazet,
                                      gaz_nt[, c("loc.correc", "is_nt")],
                                      by = c("loc.correct1" = "loc.correc"))
  result_df_gazet <- result_df_gazet[!duplicated(result_df_gazet$ids), ]
  
  to_merge <- rbind.data.frame(result_df_orig[, c("ids", "our_efg", "is_nt")],
                               result_df_gazet[, c("ids", "our_efg", "is_nt")])
  family_data <- dplyr::left_join(family_data, to_merge, by = "ids")
  
  # Is the realm transitional or predominant?
  latam_gaz_trans <- latam_gaz[grepl("^Transition", latam_gaz$Morrone_re_new), ]
  get_these <- which(family_data$loc.correct1 %in% latam_gaz_trans$loc_gaz)
  family_data$is_realm_transitional <- FALSE
  family_data$is_realm_transitional[get_these] <- TRUE
  
  latam_gaz_pred <- latam_gaz[grepl("predominant$", latam_gaz$Morrone_re_new), ]
  get_these <- which(family_data$loc.correct1 %in% latam_gaz_pred$loc_gaz)
  family_data$is_realm_predominant <- FALSE
  family_data$is_realm_predominant[get_these] <- TRUE
  
  # Doing the same for biomes
  latam_gaz_trans <- latam_gaz[grepl("^Transition", latam_gaz$our_efg_new), ]
  get_these <- which(family_data$loc.correct1 %in% latam_gaz_trans$loc_gaz)
  family_data$is_biome_transitional <- FALSE
  family_data$is_biome_transitional[get_these] <- TRUE
  
  latam_gaz_trans <- latam_gaz[grepl("predominant$", latam_gaz$our_efg_new), ]
  get_these <- which(family_data$loc.correct1 %in% latam_gaz_trans$loc_gaz)
  family_data$is_biome_predominant <- FALSE
  family_data$is_biome_predominant[get_these] <- TRUE
  
  # For still NA cases, get the info from gazetteer objects
  which_na <- which(is.na(family_data$our_efg))
  gaz_lookup <- setNames(gaz_efg$our_efg, gaz_efg$loc.correc)
  family_data$our_efg[which_na] <- gaz_lookup[family_data$loc.correct1[which_na]]
  
  which_na <- which(is.na(family_data$is_nt))
  gaz_lookup <- setNames(gaz_nt$is_nt, gaz_nt$loc.correc)
  family_data$is_nt[which_na] <- gaz_lookup[family_data$loc.correct1[which_na]]
  
  # Remove non-Neotropical endemic species
  tab <- table(family_data$scientificName, family_data$is_nt, useNA = 'ifany')
  percent_true <- tab[, "TRUE"] / rowSums(tab, na.rm = FALSE) * 100
  percent_true[is.na(percent_true)] <- 0
  true_percent <- percent_true
  
  family_data$is_nt_endemic <- family_data$scientificName %in% names(true_percent)[true_percent >= 90]
  
  # Calculate endemism per EFG
  family_data$efg_endemic <- NA
  species_list <- sort(unique(family_data$scientificName))
  
  for (j in species_list) {
    species_data <- family_data[family_data$scientificName %in% j, ]
    efg_counts <- table(species_data$our_efg)
    max_percent <- max(efg_counts) / sum(efg_counts) * 100
    
    if (max_percent >= 90) {
      dominant_efg <- names(efg_counts)[which.max(efg_counts)]
      family_data$efg_endemic[family_data$scientificName %in% j] <- dominant_efg
    }
  }
  
  # Saving by family 
  plantR::saveData(family_data,
                   dir.name = i,
                   path = file_path2,
                   by = "family",
                   file.format = "rds",
                   compress = TRUE)
  
}

# Bind all summaries together after the loop
all_filtering_summaries_df <- dplyr::bind_rows(all_filtering_summaries)

# Save the combined summary
saveRDS(all_filtering_summaries_df, 
        file.path(file_path3, "family_n_filters.rds"))

# Join duplicate-removal counts (02) with the filter counts above, by family
all_filtering_summaries_df <- readRDS(file.path(file_path3, "family_n_filters.rds"))
n_log <- as.data.frame(data.table::fread(here::here("outputs", "n_registers_log.txt")))
n_log <- n_log[n_log$step %in% c("before_dup", "after_dup"), ]
n_log <- n_log[!duplicated(n_log[, c("family", "step")], fromLast = TRUE), ]
n_log <- tidyr::pivot_wider(n_log, names_from = "step", values_from = "n_registers")
n_registers_family <- dplyr::left_join(n_log, all_filtering_summaries_df, by = "family")
n_registers_family <- dplyr::bind_rows(n_registers_family,
                                       data.frame(family = "ALL",
                                                  t(colSums(n_registers_family[, -1], na.rm = TRUE))))
# Records kept at each step (totals) and % dropped from the previous step
steps <- setdiff(names(n_registers_family)[-1], "original_n")
n_steps_total <- data.frame(step = steps,
                            n_registers = unlist(n_registers_family[n_registers_family$family %in% "ALL", steps]),
                            row.names = NULL)
n_steps_total$pct_dropped <- round(100 * (1 - n_steps_total$n_registers /
                                            c(NA, utils::head(n_steps_total$n_registers, -1))), 2)
n_steps_total$pct_of_before_dup <- round(100 * n_steps_total$n_registers / n_steps_total$n_registers[1], 2)
writexl::write_xlsx(list(by_family = n_registers_family, totals = n_steps_total),
                    here::here("outputs", "tables", "n_registers_by_family.xlsx"))
rm(steps)

# Numbers for the manuscript ----------------------------------------------
n_all <- n_registers_family[n_registers_family$family %in% "ALL", ]
dir.create(here::here("outputs", "numbers"), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(data.frame(key = c("n_records_downloaded", "n_records_nondup", "pct_records_country_level",
                                    "n_records_spatial"),
                            value = c(n_all$before_dup, n_all$after_dup,
                                      100 * sum(n_country_level) / n_all$original_n,
                                      n_all$seventh_outlier_filter),
                            description = c("Records downloaded (before removing duplicates)",
                                            "Records after removing duplicates",
                                            "% of non-duplicate records validated only at the country level",
                                            "Records in the spatial dataset")),
                 here::here("outputs", "numbers", "06_endemism_levels.csv"), row.names = FALSE)
rm(n_all, n_country_level)


