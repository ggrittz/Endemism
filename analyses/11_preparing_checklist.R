

# Checklist of tropical American trees ------------------------------------

# Cleaned occs folder
file_path1 <- here::here("data", "derived-data", "Cleaned_occs")
# Endemics occs: crème de la crème registers.
final_df <- readRDS(here::here("data", "derived-data", "rds", "final_df.rds"))

# Load data by family
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))

latam_class <- terra::vect(here::here("data", "derived-data", "maps", "latamMap_classified.gpkg"))
latam_class <- terra::as.data.frame(latam_class)

# Loading species list
our_names <- readRDS(here::here("data", "derived-data", "checklist_standardized.rds"))
our_names <- our_names[!duplicated(our_names$our_accepted_scientificName), ]


# Protochecklist for Renato
checklist <- plantR::checkList(final_df, n.vouch = 5)
# For some reason, checkList duplicates some rows
checklist <- checklist[!duplicated(checklist$scientificName), ]

# Get occ. biomes for each species
agg_biome <- aggregate(our_efg ~ scientificName, 
                       data = final_df, 
                       FUN = function(x) paste(unique(x), collapse = "|"))

# Merge this into the checklist
checklist1 <- dplyr::left_join(checklist, agg_biome, 
                               by = "scientificName")

# Names too
checklist1 <- dplyr::left_join(checklist1, our_names[, c("our_accepted_scientificName",
                                                         "our_accepted_bb",
                                                         "our_acceptedID")],
                               by = c("scientificName" = "our_accepted_scientificName"))

saveRDS(checklist1, file.path("data", "derived-data", "rds", "proto_checklist.rds"))

# Prepare which countries/states can be used for specimens
# e.g., species fall into Brazil, so it is endemic to Neotropics
# # state-level
latam_class$state_clean <- paste(latam_class$NAME_0, latam_class$NAME_1, sep = "_")

# Get unique our_efg aggregations by cleaned location
agg_state <- aggregate(our_efg_new ~ state_clean, 
                       data = latam_class, 
                       FUN = function(x) paste(unique(x), collapse = "|"))
agg_state <- agg_state[!grepl("\\|", agg_state$our_efg_new), ]
agg_state <- agg_state[!grepl("predominant$", agg_state$our_efg_new), ]
names(agg_state)[2] <- "our_efg_state_level"
# latam_class <- dplyr::left_join(latam_class, agg, by = "state_clean")

##### Do the same at country-level now #####
agg_country <- aggregate(our_efg_new ~ NAME_0,
                         data = latam_class,
                         FUN = function(x) paste(unique(x), collapse = "|"))
agg_country <- agg_country[!grepl("\\|", agg_country$our_efg_new), ]
agg_country <- agg_country[!grepl("predominant$", agg_country$our_efg_new), ]
names(agg_country)[2] <- "our_efg_country_level"

# Merge back
# latam_class <- dplyr::left_join(latam_class, agg, by = "NAME_0")

##### Dot the same at municipality-level now #####
latam_class$munic_clean <- paste(latam_class$NAME_0, latam_class$NAME_1, 
                                 latam_class$NAME_2, sep = "_")

agg_munic <- aggregate(our_efg_new ~ munic_clean,
                       data = latam_class[!grepl("_NA$", latam_class$munic_clean), ],
                       FUN = function(x) paste(unique(x), collapse = "|"))
agg_munic <- agg_munic[!grepl("\\|", agg_munic$our_efg_new), ]
agg_munic <- agg_munic[!grepl("predominant$", agg_munic$our_efg_new), ]
names(agg_munic)[2] <- "our_efg_munic_level"






##### LATER #####
# Load good and bad registers
family_bad <- readRDS(file.path(file_path1, i, paste0(i, ".rds")))
family_good <- readRDS(file.path(file_path2, i, paste0(i, ".rds")))
to_use <- setdiff(family_bad$ids, family_good$ids)
family_bad <- family_bad[family_bad$ids %in% to_use, ]

# Keep only trees...
family_bad <- dplyr::left_join(family_bad, 
                                our_names[, c("our_accepted_scientificName",
                                              "lifeForm_aggregated")],
                                by = c("scientificName" = "our_accepted_scientificName"))
family_bad <- family_bad[grepl("tree", family_bad$lifeForm_aggregated), ]

# Keep only high tax confidence
family_bad <- family_bad[family_bad$tax.check1 %in% "high", ]
# Remove cultivated
family_bad <- family_bad[!family_bad$cult.check %in% c("cultivated", "prob_cultivated"), ]


# Trying to assign a biome to each of this registers ----------------------
family_bad$state_clean <- ifelse(is.na(family_bad$stateProvince.new), NA,
                                 paste0(family_bad$country.new,
                                        "_",
                                       family_bad$stateProvince.new))
family_bad <- dplyr::left_join(family_bad, agg_state,
                               by = "state_clean")
# Do the same for country-level
family_bad <- dplyr::left_join(family_bad, agg_country,
                               by = c("country.new" = "NAME_0"))




# Load by family ----------------------------------------------------------

for(i in family_names){
  cat("Loading", i, "\n")
  gz_path <- file.path(file_path1, i, paste0(i, ".csv.gz"))
  if(!file.exists(gz_path)){
    next
  }
  # Load only the specified columns
  family_data <- data.table::fread(gz_path, select = cols)
  fam_list[[i]] <- family_data
}

fam_list <- as.data.frame(dplyr::bind_rows(fam_list))

# Get growth forms for each register from our_names and keep only things that
# can occur as a tree
fam_list <- dplyr::left_join(fam_list, our_names[, c("our_accepted_scientificName",
                                                     "lifeForm_aggregated")],
                             by = c("scientificName" = "our_accepted_scientificName"))
fam_list <- fam_list[grepl("tree", fam_list$lifeForm_aggregated), ]

# Keep only Latin America/Caribbean countries
fam_list$geo.reg <- countrycode::countrycode(fam_list$country.new,
                                             origin = "country.name",
                                             destination = "region")
# Filtering only tropical American registers
fam_list <- fam_list[fam_list$geo.reg %in% c("Latin America & Caribbean"), ]

# Keep only ok_
fam_list <- fam_list[grepl("^ok_|^shore", fam_list$geo.check1), ]



##### GOTTA DO LOTS OF IFELSE #####

# Create columns to merge with state- and country-level biome classifications
fam_list$state_clean <- ifelse(!grepl("_", fam_list$loc.correct), NA,
                               sub("^([^_]+_[^_]+).*", "\\1", fam_list$loc.correct))

# Not sure if needed since I aggregated the states before...
fam_list$country_clean <- ifelse(is.na(fam_list$loc.correct), NA,
                                 sub("_.*", "", fam_list$loc.correct))

# Now merge with the biome classification for each state/country
fam_list1 <- dplyr::left_join(fam_list, gaz_class_efg[, c("loc.correc",
                                                         "our_efg_state_level")],
                              by = c("loc.correct" = "loc.correc"))





# Make checklist based on taxonomically verified registers
# table(all_data$tax.check1)
checklist <- plantR::checkList(x = fam_list,
                               n.vouch = 30,
                               type = "short",
                               rm.dup = TRUE)


#

# DO NOT FORGET ABOUT THIS!
# FOR SUMMARIZING, WE NEED THE <CHECKLIST INFO>


#############################################
#############################################
#############################################
####### Counting codes from script 08 #######
#############################################
#############################################
#############################################


##### How many families, genus, and species are endemic? #####
# length(unique(all_fam1$family)) # 179 families
# length(unique(sapply(strsplit(all_fam1$scientificName, " "), 
#                      function(x) x[1]))) # 1,714 genus
# length(unique(all_fam1$scientificName)) # 26,080 species/subspecies/varieties




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
# expanded_df$our_efg <- gsub(" predominant", "", expanded_df$our_efg)
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

if(!dir.exists(here::here("outputs", "tables")))
  dir.create(here::here("outputs", "tables"))
write.csv(result, here::here("outputs", "tables", "efg_summary_table.csv"))

# Generate species lists per EFG
species_list <- lapply(split(expanded_df, expanded_df$our_efg), 
                       function(x) unique(x$scientificName))

# Generate endemics species lists per EFG
species_list_end <- lapply(split(expanded_df, expanded_df$efg_endemic), 
                           function(x) unique(x$scientificName))

# Save these
path_to_save <- here::here("data", "derived-data", "rds")
file1 <- "spp_per_efg.rds"
file2 <- "end_per_efg.rds"
saveRDS(species_list, file.path(path_to_save, file1))
saveRDS(species_list_end, file.path(path_to_save, file2))


# What names are missing from our registers? (only good taxonomic sources here)
##### Or keep all sources anyway? #####
all_trees <- our_names1$our_accepted_scientificName[
  grepl("tree",our_names1$lifeForm_aggregated)]

##### To do this properly, we may need another filter...these are Americas' names, not #####
##### only tropical America... #####
# These registers occur in the Americas but we didn't found a good taxonomist
# to vouch for them
missing_trees <- setdiff(all_trees, unique(all_fam1$scientificName))
saveRDS(missing_trees, file.path("data", "derived-data", "rds", "missing_trees.rds"))






