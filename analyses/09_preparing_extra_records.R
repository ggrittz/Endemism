# Extra records for the checklist -------------------------------------------
# Slow step split from 10_preparing_checklist.R. Loads the cleaned records of each
# family and keeps those excluded from the spatial dataset that can still enter the
# checklist (high taxonomic confidence, not cultivated, not introduced, with country);
# assigns realm and biome by administrative unit and re-runs plantR::checkDist.
# Output: data/derived-data/rds/supp_data_for_endemics.rds (read by 10_preparing_checklist.R)

devtools::load_all()
# Cleaned occs folder
file_path1 <- here::here("data", "derived-data", "Cleaned_occs")
# Spatial dataset (records already in it are skipped)
final_df <- readRDS(here::here("data", "derived-data", "rds", "final_df_unfiltered.rds"))
# Load data by family
family_names <- readRDS(here::here("data", "derived-data", "family_names.rds"))
# Latam Map
latam_class <- terra::vect(here::here("data", "derived-data", "maps", "latamMap_classified.gpkg"))
latam_class <- terra::as.data.frame(latam_class)

# Loading species list (only trees)
our_names <- readRDS(here::here("data", "derived-data", "checklist_species_level.rds"))
our_names <- our_names[grepl("tree", our_names$lifeForm_aggregated), ]

# Loading latam adm levels reso table
latam_adm <- readRDS(here::here("data", "derived-data", "rds", "latamMap_agg_adm.rds"))
# WGSRPD table used by plantR::checkDist (reported to plantR); used only to build
# a temporary location column for checkDist below
loc_alias <- read.csv(here::here("data", "derived-data", "plantR_loc_alias.csv"))
loc_alias <- setNames(loc_alias$taxon.distribution.bru, loc_alias$loc)

# Vouchers from FFB
vouchers <- readRDS(here::here("data", "derived-data", "rds", "ffb_vouchers.rds"))
# Voucher + binomial keys (FFB uses these vouchers for species distributions)
ffb_keys <- paste(vouchers$voucher_these,
                  sub("^([^ ]+ +[^ ]+).*$", "\\1", vouchers$taxonName),
                  sep = "_")


##### RUN LATER WITH NEW PLANTR TO AVOID DOUBLE NAME AT THE END #####
# brazil_minas gerais_pocos caldas_pocos caldas
latam_class <- latam_class[latam_class$NAME_all %in% latam_adm$name, ]

# First, define which countries are 100% Neotropical
# nt_country <- aggregate(Morrone_re_new ~ NAME_0,
#                         data = latam_class,
#                         FUN = function(x) paste(unique(x), collapse = "|"))
# nt_country_string <- paste0("", nt_country$NAME_0, "", collapse = "|")

# Removing exotics
exotics <- paste(c("Mangifera", "Cocos", "Artocarpus", "Hovenia", "Eriobotrya",
                   "Eucalyptus", "Grevillea", "Ligustrum", "Pittosporum", "Syzygium",
                   "Spathodea", "Citrus", "Coffea", "Cupressus", "Thespesia", "Delonix",
                   "Terminalia catappa", "Morus nigra", "Melia azedarach", "Azadirachta indica",
                   "Dracaena fragrans", "Albizia procera", "Albizia lebbeck",
                   "Ptychosperma propinquum"), collapse = "|")

# Do the same later in bad_list and the other names too
final_df <- final_df[!grepl(exotics, final_df$scientificName), ]

bad_list <- list()

# Only relevant columns for checklisting and filtering

# Also keep the occurrenceRemarks to try to assign a biome based on description
cols = c("ids", "collectionCode.new", "catalogNumber.new", "catalogNumber", 
         "recordedBy.new", "recordNumber.new", "year.new", "family", 
         "scientificName", "taxonName", "taxonAuthor", "country.new", 
         "stateProvince.new", "municipality.new", "loc.correct1", "geo.check1", 
         "tax.check1", "out.check", "dist.check", "dup.ID", "dup.prop",
         "typeStatus", "scientific.name", "numTombo", "temp.accession", "month", 
         "day", "loc.check1", "maha.robust", "origin.coord1", "cult.check",
         "occurrenceRemarks")

# Infraspecific accepted names are moved up to their species in the Taxonomy
# project (12_species_level_checklist.R), so no taxonomy fix is needed here


# Load by family, keep what is usable ("high") and is not already on final_df and bind
for(i in family_names){
  
  cat("Loading and filtering", i, "\n")
  rds_path <- file.path(file_path1, i, paste0(i, ".rds"))
  if(!file.exists(rds_path)){
    next
  }
  
  
  
  family_data_bad <- readRDS(rds_path)
  family_data_bad <- family_data_bad[, intersect(cols, names(family_data_bad))]
  
  
  
  # Only trees
  family_data_bad <- dplyr::left_join(family_data_bad,
                                      our_names[, c("our_accepted_scientificName",
                                                    "lifeForm_aggregated")],
                                      by = c("scientificName" = "our_accepted_scientificName"))
  family_data_bad <- family_data_bad[!duplicated(family_data_bad$ids), ]
  family_data_bad <- family_data_bad[grepl("tree", family_data_bad$lifeForm_aggregated), ] 
  
  if(dim(family_data_bad)[1] == 0){
    next
  }
  
  # Records matching an FFB voucher of the same species become "high"
  occ_keys <- paste(family_data_bad$recordedBy.new, family_data_bad$recordNumber.new,
                    sub("^([^ ]+ +[^ ]+).*$", "\\1", family_data_bad$scientificName),
                    sep = "_")
  family_data_bad$tax.check1[occ_keys %in% ffb_keys] <- "high"

  # Minimum conditions to enter here:
  # Is at least tax.check1 high
  family_data_bad <- family_data_bad[family_data_bad$tax.check1 %in% "high", ]
  # Is not cultivated
  family_data_bad <- family_data_bad[family_data_bad$cult.check %in% c(NA, ""), ]
  # Is not introduced
  family_data_bad <- family_data_bad[!family_data_bad$dist.check %in% "introduced", ]
  # Has at least a country name...
  family_data_bad <- family_data_bad[!family_data_bad$country.new %in% c(NA, ""), ]
  
  if(dim(family_data_bad)[1] == 0){
    next
  }
  
  # family_data_bad$is_nt <- TRUE
  
  # Now, filter out which one of these are already in final_df and keep only registers
  # that can increase the checklist size
  family_data_good <- final_df[final_df$family %in% i, ]
  family_data_final <- family_data_bad[!family_data_bad$ids %in% family_data_good$ids, ]
  
  if(dim(family_data_final)[1] == 0){
    next
  }
  
  # Assign a biome and realm, whenever possible
  # Make my match
  family_data_final$loc.correct.state <- NA
  family_data_final$loc.correct.state <- sapply(family_data_final$loc.correct1, function(x) {
    parts <- unlist(strsplit(x, "_"))
    if (length(parts) >= 2) {
      return(paste(parts[1], parts[2], sep = "_"))
    } else {
      return(parts[1])
    }
  }, USE.NAMES = FALSE)
  
  family_data_final <- dplyr::left_join(family_data_final,
                                        latam_class[, c("NAME_all", "our_efg_new",
                                                        "Morrone_re_new")],
                                        by = c("loc.correct.state" = "NAME_all"))
  
  if(dim(family_data_final)[1] == 0){
    next
  }
  
  # Save
  family_data_final[] <- apply(family_data_final, 2, as.character)
  bad_list[[i]] <- family_data_final
}

# Bind it all
bad_list <- dplyr::bind_rows(bad_list)
bad_list$checklist_source <- "others"
names(bad_list)[which(names(bad_list) == "our_efg_new")] <- "our_efg"
# Re-run the new checkDist
bad_list$dist.check <- NULL
# New one
# Temporary location column with country/state names as written in WGSRPD
loc_state <- sub("^([^_]*_[^_]*).*", "\\1", bad_list$loc.correct1)
fix_these <- grepl("_", loc_state) & loc_state %in% names(loc_alias)
bad_list$loc.dist <- bad_list$loc.correct1
bad_list$loc.dist[fix_these] <- paste0(sub("_.*", "_", loc_state[fix_these]),
                                       loc_alias[loc_state[fix_these]],
                                       sub("^[^_]*_[^_]*", "", bad_list$loc.correct1[fix_these]))
loc_country <- sub("_.*", "", bad_list$loc.dist)
fix_these <- loc_country %in% names(loc_alias)
bad_list$loc.dist[fix_these] <- paste0(loc_alias[loc_country[fix_these]],
                                       sub("^[^_]*", "", bad_list$loc.dist[fix_these]))
bad_list <- plantR::checkDist(x = bad_list,
                              tax.name = "taxonName",
                              tax.author = "taxonAuthor",
                              loc = "loc.dist",
                              source = "wcvp")
bad_list$loc.dist <- NULL

# State not in the map (e.g. 'panama_kuna yala', 'panama_san blas islands'): use the
# country's realm (mixed countries such as Mexico or Argentina are not 'Neotropical')
no_state <- is.na(bad_list$Morrone_re_new)
bad_list$Morrone_re_new[no_state] <- latam_class$Morrone_re_new[
  match(sub("_.*", "", bad_list$loc.correct1[no_state]), latam_class$NAME_all)]

# Finally, assign the location as Neotropical or not (is_nt) based on Morrone_re_new
bad_list$is_nt <- bad_list$Morrone_re_new %in% "Neotropical"

# Drop this two columns now
bad_list$Morrone_re_new <- NULL
bad_list$our_efg_new <- NULL

# table(bad_list$is_nt, useNA = 'ifany')
##### Save again #####
saveRDS(bad_list, here::here("data", "derived-data", "rds", "supp_data_for_endemics.rds"))

rm(file_path1, final_df, family_names, latam_class, our_names, latam_adm, loc_alias,
   vouchers, ffb_keys, exotics, bad_list, cols, i, rds_path, family_data_bad, occ_keys,
   family_data_good, family_data_final, loc_state, fix_these, loc_country, no_state)
