

# Checklist of tropical American trees ------------------------------------

devtools::load_all()
# Cleaned occs folder
file_path1 <- here::here("data", "derived-data", "Cleaned_occs")
# Endemics occs: crème de la crème registers
final_df <- readRDS(here::here("data", "derived-data", "rds", "final_df_unfiltered.rds"))
# final_df_unf <- readRDS(here::here("data", "derived-data", "rds", "final_df_unfiltered.rds"))
# Load data by family
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))
# Latam Map
latam_class <- terra::vect(here::here("data", "derived-data", "maps", "latamMap_classified.gpkg"))
latam_class <- terra::as.data.frame(latam_class)
# Loading species list
our_names <- readRDS(here::here("data", "derived-data", "checklist_standardized.rds"))
our_names <- our_names[!duplicated(our_names$our_accepted_scientificName), ]
# Loading latam adm levels reso table
latam_adm <- readRDS(here::here("data", "derived-data", "rds", "latamMap_agg_adm.rds"))
# EFGs
efgs <- here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp")
efgs <- terra::vect(efgs)
efgs <- terra::project(efgs, "epsg:4326")
efgs <- efgs[efgs$Morrone_re %in% "Neotropical", ] 
biomes <- sort(unique(efgs$our_efg))


##### RUN LATER WITH NEW PLANTR TO AVOID DOUBLE NAME AT THE END #####
# brazil_minas gerais_pocos caldas_pocos caldas
latam_class <- latam_class[latam_class$NAME_all %in% latam_adm$name, ]

# First, define which countries are 100% Neotropical
# nt_country <- aggregate(Morrone_re_new ~ NAME_0,
#                         data = latam_class,
#                         FUN = function(x) paste(unique(x), collapse = "|"))
# nt_country_string <- paste0("", nt_country$NAME_0, "", collapse = "|")

# Removing exotics
exotics <- "Mangifera|Cocos|Artocarpus|Hovenia|Eriobotrya|
Eucalyptus|Grevillea|Ligustrum|Pittosporum|Syzygium|
Spathodea|Citrus|Coffea|Cupressus|Thespesia|Delonix|
Terminalia catappa|Morus nigra|Melia azedarach|Azadirachta indica|
Dracaena fragrans|Albizia procera|Albizia lebbeck|Ptychosperma propinquum"

# Do the same later in bad_list and the other names too
final_df <- final_df[!grepl(exotics, final_df$scientificName), ]

# How many species occur but are not endemic?
# Not endemics to the neotropics but occur in the neotropics!
# not_end <- sort(unique(final_df$scientificName[final_df$is_nt_endemic %in% FALSE &
#                                                      final_df$is_nt %in% TRUE]))
bad_list <- list()

##### Before anything, there are some name-issues in the checklist #####
##### We need to add them to the manual_checking_spp sheet #####
# dups <- our_names[duplicated(our_names$our_accepted_taxonName) |
#                     duplicated(our_names$our_accepted_taxonName, fromLast = TRUE), ]

# dups <- dups[, c("input_full_name", "wfo_output_full_name",
#                  "wcvp_output_full_name", "gbif_output_full_name", 
#                  "our_accepted_scientificName", "our_taxonID", 
#                  "wfo_taxonID", "wcvp_taxonID", "gbif_taxonID")]
# dups <- dups[order(dups$our_accepted_scientificName), ]
# writexl::write_xlsx(dups, here::here("data", "derived-data", "accepted_dups.xlsx"))

##### Solve above first #####

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


# Load by family, keep what is usable ("high") and is not already on final_df and bind
for(i in family_names){
  
  cat("Loading and filtering", i, "\n")
  gz_path <- file.path(file_path1, i, paste0(i, ".csv.gz"))
  if(!file.exists(gz_path)){
    next
  }
  
  family_data_bad <- as.data.frame(data.table::fread(gz_path, select = cols))
  
  # Only trees
  family_data_bad <- dplyr::left_join(family_data_bad,
                                      our_names[, c("our_accepted_scientificName",
                                                    "lifeForm_aggregated")],
                                      by = c("scientificName" = "our_accepted_scientificName"))
  family_data_bad <- family_data_bad[grepl("tree", family_data_bad$lifeForm_aggregated), ] 
  
  if(dim(family_data_bad)[1] == 0){
    next
  }
  
  # Minimum conditions to enter here:
  # Is at least tax.check1 high
  family_data_bad <- family_data_bad[family_data_bad$tax.check1 %in% "high", ]
  # Is not cultivated
  family_data_bad <- family_data_bad[family_data_bad$cult.check %in% "", ]
  # Is not introduced
  family_data_bad <- family_data_bad[!family_data_bad$dist.check %in% "introduced", ]
  # Has at least a country name...
  family_data_bad <- family_data_bad[!family_data_bad$country.new %in% "", ]
  
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
bad_list <- my_checkDist(x = bad_list,
                        tax.name = "taxonName",
                        tax.author = "taxonAuthor",
                        loc = "loc.correct1",
                        source = "wcvp")

# Finally, assign the location as Neotropical or not (is_nt) based on Morrone_re_new
bad_list$is_nt <- bad_list$Morrone_re_new %in% "Neotropical"

# Drop this two columns now
bad_list$Morrone_re_new <- NULL
bad_list$our_efg_new <- NULL

# table(bad_list$is_nt, useNA = 'ifany')
##### Save again #####
saveRDS(bad_list, here::here("data", "derived-data", "rds", "supp_data_for_endemics.rds"))


# New start here! ---------------------------------------------------------
bad_list <- readRDS(here::here("data", "derived-data", "rds", "supp_data_for_endemics.rds"))
# Since these are the 'bad for interpolation' cases, we are keeping only ok dist here...
# This step will also filter out introduced specimens not indicated in POWO
bad_list <- bad_list[bad_list$dist.check %in% "ok_dist", ]
 
# Any of these are really small countries/states? In these cases, we keep everything
bad_list <- dplyr::left_join(bad_list, latam_adm,
                             by = c("loc.correct1" = "name"))

# Removing exotics from bad_list
bad_list <- bad_list[!grepl(exotics, bad_list$scientificName), ]
# drop loc.correct.state
bad_list$loc.correct.state <- NULL

# Merge both dfs
to_keep <- names(bad_list)
final_df$checklist_source <- "interpolation"
final_df1 <- final_df[, to_keep]
final_df1$is_nt <- as.logical(final_df1$is_nt)
final_df1$bad_reso <- as.logical(final_df1$bad_reso)
final_df_all <- dplyr::bind_rows(final_df1, bad_list)

##### NEOTROPICAL ENDEMICS! #####
# 90% rule for now
tab <- table(final_df_all$scientificName, final_df_all$is_nt, useNA = 'ifany')
percent_true <- tab[, "TRUE"] / rowSums(tab, na.rm = FALSE) * 100
percent_true[is.na(percent_true)] <- 0
true_percent <- percent_true

# Get names where ≥ 90% occurrences are within the Neotropics
# aka Neotropical endemics
keep_species <- names(true_percent)[true_percent >= 90]
final_df_all$spp_is_nt_endemic <- final_df_all$scientificName %in% names(true_percent)[true_percent >= 90]
# bad_list <- bad_list[bad_list$scientificName %in% keep_species, ]
##### These ones will be used to overall tropical American values! #####


##### Use this for richness per biome #####
# First, remove all transition cases
get_these <- !grepl("Transition", final_df_all$our_efg)
final_df_richness_biome <- final_df_all[get_these, ]
# Now, remove bad resolution (too coarse to assume it is predominant in a location)
# Areas above 12,500 km² = 5 grid cells of 50x50 km
get_these <- which(final_df_richness_biome$bad_reso %in% c(FALSE, NA))
final_df_richness_biome <- final_df_richness_biome[get_these, ]

# And consider predominant the biome per se
final_df_richness_biome$our_efg <- gsub(" predominant", "", final_df_richness_biome$our_efg)

# Columns to be used later
# keep_cols <- names(bad_list)


# Endemism per efg, recalculated
final_df_endemism <- final_df_all[!grepl("Transition|predominant", 
                                              final_df_all$our_efg), ]
# Also remove bad resolution cases
final_df_endemism <- final_df_endemism[final_df_endemism$bad_reso %in% c(NA, FALSE), ]

# Populate this
final_df_endemism$spp_is_efg_endemic <- NA

# Get unique species per EFG and count (same 90% rule)
species_list <- sort(unique(final_df_endemism$scientificName))

for (j in species_list) {
  cat("Endemism level of", j, "\n")
  species_data <- final_df_endemism[final_df_endemism$scientificName %in% j, ]
  efg_counts <- table(species_data$our_efg)
  max_percent <- max(efg_counts) / sum(efg_counts) * 100
  
  # If ≥ 90%, assign the dominant EFG
  if (max_percent >= 90) {
    dominant_efg <- names(efg_counts)[which.max(efg_counts)]
    final_df_endemism$spp_is_efg_endemic[final_df_endemism$scientificName %in% j] <- dominant_efg
  }
}
# saveRDS(final_df_endemism, here::here("data", "derived-data", "rds", "data_for_endemics.rds"))


##### CONTINUAR A PARTIR DO SPP_IS_NT_ENDEMIC #####
final_df_endemism <- readRDS(here::here("data", "derived-data", "rds", "data_for_endemics.rds"))

# Binding both cases for checklisting -------------------------------------

# # Only necessary cols
# final_df$checklist_source <- "interpolation"
# final_df <- final_df[, c(to_keep, "efg_endemic", "spp_is_nt_endemic")]
# final_df$bad_reso <- as.logical(final_df$bad_reso)
# final_df$is_nt <- as.logical(final_df$is_nt)
# final_df$is_nt_endemic <- as.logical(final_df$is_nt_endemic)

# Summary of tropical America results: species, families, genus...
# This is the tropical America checklist per se, i.e., species that occur here
# and are not exotic
# final_df_all <- dplyr::bind_rows(final_df, bad_list)

# This will be used for richness per biome # Don't forget to summarise these too!
# the same for endemism...
# final_df_richness_biome <- dplyr::bind_rows(final_df, bad_list_richness_biome)

# For endemism
# final_df_endemism <- dplyr::bind_rows(final_df, bad_list_endemism)

##### Removing synonyms from accepted names for all these mini dfs #####

# Any name in my current list of names that is not an accepted name?
all_spp <- sort(unique(final_df_all$scientificName))
all_spp <- plantR::fixAuthors(all_spp)
names(all_spp) <- c("orig.name", "scientificName", "scientificNameAuthorship")
data("wfoNames", package = 'plantRdata')
all_spp <- plantR::prepSpecies(x = all_spp, 
                               # tax.names = c("tax.name", "tax.author"),
                               db = wfoNames,
                               split.letters = TRUE,
                               mult.matches = "best",
                               show.progress = TRUE,
                               drop.cols = "")

# Get the synonyms and see if their accepted name is actually in my list
syn_spp <- all_spp$scientificNameFull[all_spp$taxon.status %in% "synonym"]
# Are all the accepted names from these synonyms in my list?
any(!syn_spp %in% all_spp$scientificNameFull) # yes, so we can remove these synonyms...

# Removing them from both df
syn_spp <- all_spp$orig.name[all_spp$taxon.status %in% "synonym"]
final_df_all <- final_df_all[!final_df_all$scientificName %in% syn_spp, ]
final_df_richness_biome <- final_df_richness_biome[!final_df_richness_biome$scientificName %in% syn_spp, ]
final_df_endemism <- final_df_endemism[!final_df_endemism$scientificName %in% syn_spp, ]

# Keeping only tax.check1 == high in all dfs...
final_df_all <- final_df_all[final_df_all$tax.check1 %in% "high", ]
final_df_richness_biome <- final_df_richness_biome[final_df_richness_biome$tax.check1 %in% "high", ]
final_df_endemism <- final_df_endemism[final_df_endemism$tax.check1 %in% "high", ]


##### How many native species, genus, and families we have in tropical America? #####
# How many registers from each type of source?
nrow(final_df_all) # 1,824,362 registers used

# How many species occur in the tropical America
length(unique(final_df_all$scientificName[final_df_all$is_nt %in% TRUE])) # we have 27,607 species
length(unique(final_df_all$scientificName[final_df_all$is_nt %in% TRUE & # of which 26,090 are endemic
                                            final_df_all$spp_is_nt_endemic %in% TRUE]))

length(unique(gsub(" .*", "", final_df_all$scientificName[final_df_all$is_nt %in% TRUE]))) # 1,941 genera
length(unique(final_df_all$family[final_df_all$is_nt %in% TRUE])) # and 196 families

# Not endemics: check these
non_end <- unique(final_df_all$scientificName[final_df_all$is_nt %in% TRUE &
                                                final_df_all$spp_is_nt_endemic %in% FALSE])
non_end_df <- final_df_all[final_df_all$scientificName %in% non_end, ]


##### Species that we could not assign a biome #####
missing_spp <- sort(setdiff(final_df_all$scientificName, 
                            final_df_richness_biome$scientificName))

# Subset them
missing_spp_df <- final_df_all[final_df_all$scientificName %in% missing_spp, ]

# Load data from Flora do Brasil and see if we can assign a ecoregion/biome
file_path <- file.path("D:", "Doutorado", "SynTreeSys", "Taxonomy",
                               "data", "raw-data")
file_name <- "trees_and_shrubs_flora_do_brasil.rds"
fbo <- readRDS(file.path(file_path, file_name))
fbo$scientificName <- paste(fbo$taxonName, fbo$taxonAuthorship)

# Merge FBO vegetationType info with the missing_spp_df
missing_spp_df <- dplyr::left_join(missing_spp_df,
                                   fbo[, c("scientificName", "vegetationType")],
                                   by = "scientificName")

# Merge domain info from taxonomy too
missing_spp_df <- dplyr::left_join(missing_spp_df,
                                   our_names[, c("our_accepted_scientificName",
                                                 "domain_aggregated")],
                                   by = c("scientificName" = "our_accepted_scientificName"))

# First, remove transition cases
##### IF WE HAVE INFO FROM FBO SHOULD WE KEEP THEM? #####
missing_spp_df <- missing_spp_df[!grepl("Transition", missing_spp_df$our_efg), ]

# Preparing column domain_aggregated to then use if with vegetationType
split_list <- strsplit(missing_spp_df$domain_aggregated, "\\|")

cleaned_list <- lapply(split_list, function(parts) {
  parts <- trimws(parts)
  parts <- parts[parts != ""]
  non_na_parts <- parts[parts != "NA"]
  if (length(non_na_parts) == 0) "NA" else paste(non_na_parts, collapse = "|")
})

# Assigning back
missing_spp_df$domain_cleaned <- unlist(cleaned_list)
missing_spp_df$domain_cleaned <- gsub("^NA$", NA, missing_spp_df$domain_cleaned)

# Assign the biome when there is no doubt the domain indicates == a single biome
# e.g: los llanos is 100% savannas
# domains = data.frame(table(unlist(strsplit(missing_spp_df$domain_cleaned, 
#                                            split = "\\|")), useNA = 'ifany'))
# Caatinga is dry forest
missing_spp_df$domain_cleaned <- gsub("Caatinga", 
                                      "Tropical-subtropical dry forests and thickets",
                                      missing_spp_df$domain_cleaned)
# Gran Chaco is dry forest
missing_spp_df$domain_cleaned <- gsub("Gran Chaco", 
                                      "Tropical-subtropical dry forests and thickets",
                                      missing_spp_df$domain_cleaned)
# Cerrado is savanna
missing_spp_df$domain_cleaned <- gsub("Cerrado", 
                                      "Pyric tussock savannas",
                                      missing_spp_df$domain_cleaned)
# Los Llanos are savannas
missing_spp_df$domain_cleaned <- gsub("Los Llanos", 
                                      "Pyric tussock savannas",
                                      missing_spp_df$domain_cleaned)
# Pampa are temperate subhumid grasslands 
missing_spp_df$domain_cleaned <- gsub("Pampa", 
                                      "Temperate subhumid grasslands",
                                      missing_spp_df$domain_cleaned)
# Peruvian-Chilean Desert is the Atacama, i.e., hyper arid desert
missing_spp_df$domain_cleaned <- gsub("Peruvian-Chilean Desert", 
                                      "Hyper-arid deserts",
                                      missing_spp_df$domain_cleaned)

##### Using FBO for the same purpose now #####
sort(table(unlist(strsplit(missing_spp_df$vegetationType, 
                           split = "\\|")), useNA = 'ifany'))

# Rename vegetation types based on biome
missing_spp_df$vegetationType <- gsub("Campo de Várzea", "Tropical flooded forests and peat forests", missing_spp_df$vegetationType)
# Dry, lowlands or ignore? Possibly ignoring...
# missing_spp_df$vegetationType <- gsub("Floresta Estacional Semidecidual", "", missing_spp_df$vegetationType)
missing_spp_df$vegetationType <- gsub("Cerrado (lato sensu)", "Pyric tussock savannas", missing_spp_df$vegetationType, fixed = TRUE)
missing_spp_df$vegetationType <- gsub("Floresta Ombrófila (= Floresta Pluvial)", "Tropical-subtropical lowland rainforests", 
                                      missing_spp_df$vegetationType, fixed = TRUE)
# Campo Limpo can be Pampa or Cerrado
# missing_spp_df$vegetationType <- gsub("Campo Limpo", "Pyric tussock savannas", missing_spp_df$vegetationType)
# Carrasco can be both Cerrado and Caatinga
# missing_spp_df$vegetationType <- gsub("Carrasco", "", missing_spp_df$vegetationType)
missing_spp_df$vegetationType <- gsub("Campo rupestre", "Seasonally dry tropical shrublands", missing_spp_df$vegetationType)
missing_spp_df$vegetationType <- gsub("Floresta Ombrófila Mista", "Warm temperate laurophyll forests", missing_spp_df$vegetationType)
missing_spp_df$vegetationType <- gsub("Floresta Estacional Decidual", "Tropical-subtropical dry forests and thickets", missing_spp_df$vegetationType)
missing_spp_df$vegetationType <- gsub("Floresta de Igapó", "Tropical flooded forests and peat forests", missing_spp_df$vegetationType)
missing_spp_df$vegetationType <- gsub("Floresta de Várzea", "Tropical flooded forests and peat forests", missing_spp_df$vegetationType)
missing_spp_df$vegetationType <- gsub("Floresta de Terra Firme", "Tropical-subtropical lowland rainforests", missing_spp_df$vegetationType)
missing_spp_df$vegetationType <- gsub("Floresta Estacional Perenifólia", "Tropical-subtropical lowland rainforests", missing_spp_df$vegetationType)
# Can't classify. Mixed between multiple biomes...
# missing_spp_df$vegetationType <- gsub("Campo de Altitude", "Seasonally dry tropical shrublands", missing_spp_df$vegetationType)
missing_spp_df$vegetationType <- gsub("Caatinga (stricto sensu)", "Tropical-subtropical dry forests and thickets", 
                                      missing_spp_df$vegetationType, fixed = TRUE)
missing_spp_df$vegetationType <- gsub("Restinga", "Tropical heath forests", missing_spp_df$vegetationType)
missing_spp_df$vegetationType <- gsub("Savana Amazônica", "Pyric tussock savannas", missing_spp_df$vegetationType)
missing_spp_df$vegetationType <- gsub("Campinarana", "Tropical heath forests", missing_spp_df$vegetationType)

# Filter each list element to only keep values that are in 'biomes'
domain_split <- strsplit(missing_spp_df$domain_cleaned, split = "\\|")
filtered_domains <- lapply(domain_split, function(x) {
  valid_domains <- x[!is.na(x) & x %in% biomes]
  if (length(valid_domains) == 0) {
    return(NA)
  } else {
    return(valid_domains)
  }
})

# Collapse back into pipe-separated strings
missing_spp_df$domain_cleaned <- sapply(filtered_domains, function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(paste(x, collapse = "|"))
  }
})

# Same for vegetationType
veg_split <- strsplit(missing_spp_df$vegetationType, split = "\\|")
filtered_veg <- lapply(veg_split, function(x) {
  valid_veg <- x[!is.na(x) & x %in% biomes]
  if (length(valid_veg) == 0) {
    return(NA)
  } else {
    return(valid_veg)
  }
})

missing_spp_df$vegetationType <- sapply(filtered_veg, function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    return(paste(x, collapse = "|"))
  }
})

# Keep only species which we were able to assign a biom
missing_spp_df1 <- missing_spp_df[!is.na(missing_spp_df$vegetationType) |
                                    !is.na(missing_spp_df$domain_cleaned), ]
# Repopulate column our_efg with the new probable biome from both sources
missing_spp_df1$our_efg <- apply(missing_spp_df1[, c("vegetationType", "domain_cleaned")], 
                                 1, function(x) {
  veg <- x[1]
  dom <- x[2]
  
  veg_vec <- if (!is.na(veg)) unlist(strsplit(veg, "\\|")) else character(0)
  dom_vec <- if (!is.na(dom)) unlist(strsplit(dom, "\\|")) else character(0)
  
  all_vals <- c(veg_vec, dom_vec)
  unique_vals <- unique(all_vals)
  unique_vals <- unique_vals[unique_vals != "" & !is.na(unique_vals)]
  
  if (length(unique_vals) == 0) {
    NA_character_
  } else {
    paste(unique_vals, collapse = "|")
  }
})

# Addback into final_df_richness_biome. Can't use this for endemism, I think
missing_spp_df1 <- missing_spp_df1[, to_keep]
# missing_spp_df1$efg_endemic <- NA


# How many species for each biome? ----------------------------------------
final_df_richness_biome <- plyr::rbind.fill(final_df_richness_biome,
                                            missing_spp_df1)

# TO REMIND ME: I can use occurrenceRemarks to mine a bit more... #
# We were able to assign a biome to 24,116 species
biome_richness <- data.frame(scientificName = final_df_richness_biome$scientificName,
                             our_efg = final_df_richness_biome$our_efg)
biome_richness <- tidyr::separate_longer_delim(biome_richness,
                                               our_efg,
                                               delim = "|")
biome_richness_agg <- aggregate(scientificName ~ our_efg,
                            data = biome_richness,
                            FUN = function(x) length(unique(x)))
names(biome_richness_agg)[which(names(biome_richness_agg) == "scientificName")] <- "richness"

##### Now, do the same for endemics! #####
biome_endemics <- aggregate(scientificName ~ spp_is_efg_endemic,
                            data = final_df_endemism,
                            FUN = function(x) length(unique(x)))
names(biome_endemics)[which(names(biome_endemics) == "scientificName")] <- "endemics"

# Join to get endemism levels inside each biome
biome_tab <- dplyr::left_join(biome_richness_agg, biome_endemics,
                              by = c("our_efg" = "spp_is_efg_endemic"))
biome_tab$end_lvl <- 100 * (biome_tab$endemics / biome_tab$richness)
write.csv(biome_tab, file.path("outputs", "tables", "efg_tab.csv"))

##### Biome summary #####
# Most endemic biomes are lowland/montane/thickets
# How much do they represent on single-biome endemics?

sum(biome_tab$endemics[grepl("lowland|montane|thickets", biome_tab$our_efg)]) / 
  sum(biome_tab$endemics)


# Checklisting ------------------------------------------------------------
full_df1 <- final_df_all
# This is used later for spec acc curve
neotropical_species <- unique(final_df_all$scientificName[final_df_all$spp_is_nt_endemic %in% TRUE])
saveRDS(neotropical_species, here::here("data", "derived-data", "nt_species.rds"))
full_df1$scientificName <- NULL
names(full_df1)[which(names(full_df1) == "taxonName")] <- "scientificName"
names(full_df1)[which(names(full_df1) == "taxonAuthor")] <- "scientificNameAuthorship"
checklist <- plantR::checkList(x = full_df1, n.vouch = 10, rm.dup = FALSE)
checklist <- checklist[!duplicated(checklist$scientificName), ]

##### FIX THS LATER! do not edit over the biome_richness object so i can use it here

# Generate species lists per EFG
species_list_richness <- lapply(split(biome_richness, biome_richness$our_efg), 
                       function(x) unique(x$scientificName))

# Generate endemics species lists per EFG
species_list_endemics <- lapply(split(final_df_endemism, final_df_endemism$spp_is_efg_endemic), 
                           function(x) unique(x$scientificName))

# Save these
path_to_save <- here::here("data", "derived-data", "rds")
file1 <- "spp_per_efg.rds"
file2 <- "end_per_efg.rds"
saveRDS(species_list_richness, file.path(path_to_save, file1))
saveRDS(species_list_endemics, file.path(path_to_save, file2))


# What names are missing from our registers? (only good taxonomic sources here)
##### Or keep all sources anyway? #####
all_trees <- our_names$our_accepted_scientificName[
  grepl("tree",our_names$lifeForm_aggregated)]



