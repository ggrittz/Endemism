# Checklist of tropical American trees ------------------------------------
# The slow family-by-family step that builds the extra records (records excluded from
# the spatial dataset) is in 10.0_preparing_extra_records.R, which must be run
# before this script; its output (supp_data_for_endemics.rds) is read below

devtools::load_all()
# Endemics occs: crème de la crème registers
final_df <- readRDS(here::here("data", "derived-data", "rds", "final_df_unfiltered.rds"))

# Loading species list
our_names <- readRDS(here::here("data", "derived-data", "checklist_species_level.rds"))
# Only trees
our_names <- our_names[grepl("tree", our_names$lifeForm_aggregated), ]
# Only accepted names below
sum(!duplicated(our_names$our_accepted_scientificName)) 
aggregate(our_accepted_scientificName ~ our_accepted_bb,
          data = our_names[!duplicated(our_names$our_accepted_scientificName),],
          FUN = length) 

##### Fast summarization of this data to add to paper table S2 #####
to_summ <- our_names[, c("source", "input_full_name", "our_accepted_scientificName")]

# For total names
source_list <- strsplit(to_summ$source, split = " \\| ")
all_names <- rep(to_summ$input_full_name, lengths(source_list))
sources <- unlist(source_list)
# Before anything, merge G+G&C, TreeAtlan+NTT+NeoTropTree2014, 
# our_checklist+extraSource_DKTN+GBIF
sources <- gsub("^G$", "G and G&C", sources)
sources <- gsub("^GC$", "G and G&C", sources)
sources <- gsub("^NTT$", "NeoTropTree", sources)
sources <- gsub("^NeoTropTree 2014$", "NeoTropTree", sources)
sources <- gsub("^TreeAtlan 2013$", "NeoTropTree", sources)
sources <- gsub("^GBIF$|^our_checklist$|^extraSource_DKTN$", "Others", sources)
table(sources)

expanded_df <- data.frame(all_names = all_names,
                          source = sources,
                          stringsAsFactors = FALSE)

# For total accepted names
accepted_names <- rep(to_summ$our_accepted_scientificName, lengths(source_list))
expanded_df_acc <- data.frame(accepted_names = accepted_names,
                              source = sources,
                              stringsAsFactors = FALSE)

# Calculate total counts per source for input names
total_counts <- as.data.frame(table(expanded_df$source))
names(total_counts) <- c("source", "total_count")

# Calculate unique input names (names that appear in only one source)
name_counts <- as.data.frame(table(expanded_df$all_names))
name_counts$Var1 <- as.character(name_counts$Var1)
single_source_names <- name_counts$Var1[name_counts$Freq == 1]

single_source_rows <- expanded_df[expanded_df$all_names %in% single_source_names, ]
unique_counts <- as.data.frame(table(single_source_rows$source))
names(unique_counts) <- c("source", "unique_input_count")

# Calculate unique accepted names (accepted names that appear in only one source)
name_counts_acc <- as.data.frame(table(expanded_df_acc$accepted_names))
name_counts_acc$Var1 <- as.character(name_counts_acc$Var1)
single_source_names_acc <- name_counts_acc$Var1[name_counts_acc$Freq == 1]
single_source_rows_acc <- expanded_df_acc[expanded_df_acc$accepted_names %in% single_source_names_acc, ]
unique_counts_acc <- as.data.frame(table(single_source_rows_acc$source))
names(unique_counts_acc) <- c("source", "unique_accepted_count")

# Final table
source_summary <- dplyr::left_join(total_counts, unique_counts, by = "source")
source_summary <- dplyr::left_join(source_summary, unique_counts_acc, by = "source")
source_summary$unique_input_count[is.na(source_summary$unique_input_count)] <- 0
source_summary$unique_accepted_count[is.na(source_summary$unique_accepted_count)] <- 0
source_summary <- source_summary[order(-source_summary$total_count), ]
# source_summary

# Ordering by good/less good source
writexl::write_xlsx(source_summary, here::here("outputs", "tables", "sources_taxonomy_summary.xlsx"))

# How many tree names?
table(our_names$our_accepted_bb[grepl("tree", our_names$lifeForm_aggregated)])

# Loading latam adm levels reso table
latam_adm <- readRDS(here::here("data", "derived-data", "rds", "latamMap_agg_adm.rds"))
# EFGs
efgs <- here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp")
efgs <- terra::vect(efgs)
efgs <- terra::project(efgs, "epsg:4326")
efgs <- efgs[efgs$Morrone_re %in% "Neotropical", ]
biomes <- sort(unique(efgs$our_efg))

# Removing exotics (same list as in 10.0_preparing_extra_records.R)
exotics <- paste(c("Mangifera", "Cocos", "Artocarpus", "Hovenia", "Eriobotrya",
                   "Eucalyptus", "Grevillea", "Ligustrum", "Pittosporum", "Syzygium",
                   "Spathodea", "Citrus", "Coffea", "Cupressus", "Thespesia", "Delonix",
                   "Terminalia catappa", "Morus nigra", "Melia azedarach", "Azadirachta indica",
                   "Dracaena fragrans", "Albizia procera", "Albizia lebbeck",
                   "Ptychosperma propinquum"), collapse = "|")

# Do the same later in bad_list and the other names too
final_df <- final_df[!grepl(exotics, final_df$scientificName), ]

# New start here! ---------------------------------------------------------
bad_list <- readRDS(here::here("data", "derived-data", "rds", "supp_data_for_endemics.rds"))
# Since these are the 'bad for interpolation' cases, we are keeping only ok dist here...
# This step will also filter out introduced specimens not indicated in POWO
##### Putting this back! 04/03/2026 #####
# Log: extra candidates before the ok_dist filter
data.table::fwrite(data.frame(family = "ALL", step = "11_extra_candidates",
                              n_registers = nrow(bad_list)),
                   here::here("outputs", "n_registers_log.txt"), append = TRUE, sep = "\t")
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


# Recreate scientificName column
final_df_all$scientificName <- ifelse(
  !is.na(final_df_all$taxonAuthor) & final_df_all$taxonAuthor != "",
  paste(final_df_all$taxonName, final_df_all$taxonAuthor),
  final_df_all$taxonName
)

# How many infraspecific names on scientificName?

# test_them <- final_df_all[, ]
# Remove these. They are unknown/unchecked/bullshit names from GBIF
# 300 registers from 18 species
final_df_all <- final_df_all[!grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                                    final_df_all$taxonName, 
                                    perl = TRUE), ]


##### NEOTROPICAL ENDEMICS! #####
# 90% rule for now
tab <- table(final_df_all$scientificName, final_df_all$is_nt, useNA = 'ifany')
percent_true <- tab[, "TRUE"] / rowSums(tab, na.rm = FALSE) * 100
percent_true[is.na(percent_true)] <- 0
true_percent <- percent_true

# To get occurrences and proportion
tab_df <- as.data.frame(tab)
tab_df <- tab_df[order(tab_df$Var1), ]
tab_df <- tab_df[!is.na(tab_df$Var2), ]
# Reshape...
tab_wide <- reshape(as.data.frame(tab_df),
                    idvar = "Var1",
                    timevar = "Var2",
                    direction = "wide")
names(tab_wide) <- c("spp_name", "occ_outside_nt", "occ_inside_nt")
tab_wide$endemism_level_nt <- round(100 * (tab_wide$occ_inside /
                                             (tab_wide$occ_outside + tab_wide$occ_inside)),
                                    2)
tab_wide <- tab_wide[!is.nan(tab_wide$endemism_level_nt), ]

# To populate
final_df_all$spp_is_nt_endemic <- NA_character_
# Pure-endemics (100%)
pure_endemic_species <- names(true_percent)[true_percent == 100]
# Near-endemics (≥ 90% but < 100%)
near_endemic_species <- names(true_percent)[true_percent >= 90 & true_percent < 100]
# Assign categories
final_df_all$spp_is_nt_endemic[final_df_all$scientificName %in% pure_endemic_species] <- "pure-endemic"
final_df_all$spp_is_nt_endemic[final_df_all$scientificName %in% near_endemic_species] <- "near-endemic"
final_df_all$spp_is_nt_endemic[final_df_all$scientificName %in% names(true_percent)[true_percent < 90]] <- "not endemic"

# Add the registers count
final_df_all <- dplyr::left_join(final_df_all, tab_wide, by = c("scientificName" = "spp_name"))

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
# Remove bad_dist from here, insteadof removing from final_df_All
# Only for the extra (less curated) records; 07 records already passed the outlier rule
final_df_richness_biome <- final_df_richness_biome[final_df_richness_biome$checklist_source %in% "interpolation" |
                                                     final_df_richness_biome$dist.check %in% "ok_dist", ]

# Columns to be used later
# keep_cols <- names(bad_list)

# Endemism per efg, recalculated
final_df_endemism <- final_df_all[!grepl("Transition|predominant", 
                                              final_df_all$our_efg), ]
# Also remove bad resolution cases
final_df_endemism <- final_df_endemism[final_df_endemism$bad_reso %in% c(NA, FALSE), ]

# Remove bad_dist from here (only for the extra records, as above)
final_df_endemism <- final_df_endemism[final_df_endemism$checklist_source %in% "interpolation" |
                                         final_df_endemism$dist.check %in% "ok_dist", ]

# Populate this
final_df_endemism$spp_is_efg_endemic <- NA_character_

# Get unique species per EFG and count (same 90% rule)
species_list <- sort(unique(final_df_endemism$scientificName))

data.table::setDT(final_df_endemism)

final_df_endemism[, spp_is_efg_endemic := 
                    {
                      efg_counts <- table(our_efg)
                      max_percent <- max(efg_counts) / sum(efg_counts) * 100
                      dominant_efg <- names(which.max(efg_counts))
                      
                      if (max_percent == 100) {
                        paste(dominant_efg, "pure-endemic")
                      } else if (max_percent >= 90) {
                        paste(dominant_efg, "near-endemic")
                      } else {
                        "not endemic"
                      }
                    }, 
                  by = scientificName]

final_df_endemism <- as.data.frame(final_df_endemism)
# Log: spatial, extra and checklist datasets, and the biome-level subsets
data.table::fwrite(data.frame(family = "ALL",
                              step = c("11_spatial", "11_extra_ok_dist", "11_checklist_all",
                                       "11_richness_biome_spatial", "11_richness_biome_extra",
                                       "11_endemism_biome_spatial", "11_endemism_biome_extra"),
                              n_registers = c(sum(final_df_all$checklist_source %in% "interpolation"),
                                              sum(final_df_all$checklist_source %in% "others"),
                                              nrow(final_df_all),
                                              sum(final_df_richness_biome$checklist_source %in% "interpolation"),
                                              sum(final_df_richness_biome$checklist_source %in% "others"),
                                              sum(final_df_endemism$checklist_source %in% "interpolation"),
                                              sum(final_df_endemism$checklist_source %in% "others"))),
                   here::here("outputs", "n_registers_log.txt"), append = TRUE, sep = "\t")
saveRDS(final_df_endemism, here::here("data", "derived-data", "rds", "data_for_endemics.rds"))


##### CONTINUAR A PARTIR DO SPP_IS_NT_ENDEMIC #####
# final_df_endemism <- readRDS(here::here("data", "derived-data", "rds", "data_for_endemics.rds"))

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
                               drop.cols = "",
                               sug.dist = 0.85)

synonym_lookup <- all_spp[all_spp$taxon.status %in% "synonym", ]

##### Syn to accepted: all lookups here #####
# taxonName
orig_to_suggestedName <- setNames(synonym_lookup$suggestedName,
                                  synonym_lookup$orig.name)
# taxonAuthor
orig_to_suggestedAuthorship <- setNames(synonym_lookup$suggestedAuthorship,
                                  synonym_lookup$orig.name)
# scientificName
orig_to_scientificNameFull <- setNames(synonym_lookup$scientificNameFull,
                                        synonym_lookup$orig.name)
# family
orig_to_suggestedFamily <- setNames(synonym_lookup$suggestedFamily,
                                       synonym_lookup$orig.name)

# Get them
get_these <- which(final_df_all$scientificName %in% names(orig_to_suggestedName))

# Finally, substitute in all dfs 
final_df_all$taxonName[get_these] <- orig_to_suggestedName[final_df_all$scientificName[get_these]]
final_df_all$taxonAuthor[get_these] <- orig_to_suggestedAuthorship[final_df_all$scientificName[get_these]]
final_df_all$family[get_these] <- orig_to_suggestedFamily[final_df_all$scientificName[get_these]]
final_df_all$scientificName[get_these] <- orig_to_scientificNameFull[final_df_all$scientificName[get_these]]

# From richness dataset
get_these <- which(final_df_richness_biome$scientificName %in% names(orig_to_suggestedName))
final_df_richness_biome$taxonName[get_these] <- orig_to_suggestedName[final_df_richness_biome$scientificName[get_these]]
final_df_richness_biome$taxonAuthor[get_these] <- orig_to_suggestedAuthorship[final_df_richness_biome$scientificName[get_these]]
final_df_richness_biome$family[get_these] <- orig_to_suggestedFamily[final_df_richness_biome$scientificName[get_these]]
final_df_richness_biome$scientificName[get_these] <- orig_to_scientificNameFull[final_df_richness_biome$scientificName[get_these]]
# From endemism dataset
get_these <- which(final_df_endemism$scientificName %in% names(orig_to_suggestedName))
final_df_endemism$taxonName[get_these] <- orig_to_suggestedName[final_df_endemism$scientificName[get_these]]
final_df_endemism$taxonAuthor[get_these] <- orig_to_suggestedAuthorship[final_df_endemism$scientificName[get_these]]
final_df_endemism$family[get_these] <- orig_to_suggestedFamily[final_df_endemism$scientificName[get_these]]
final_df_endemism$scientificName[get_these] <- orig_to_scientificNameFull[final_df_endemism$scientificName[get_these]]

# Keeping only tax.check1 == high in all dfs...
final_df_all <- final_df_all[final_df_all$tax.check1 %in% "high", ]
final_df_richness_biome <- final_df_richness_biome[final_df_richness_biome$tax.check1 %in% "high", ]
final_df_endemism <- final_df_endemism[final_df_endemism$tax.check1 %in% "high", ]

# Records of the checklist dataset for the Zenodo deposit (13_zenodo_export.R):
# dataset = "spatial" (spatial dataset) or "extra" (assigned through the
# administrative unit only). Only the columns present in final_df_all are kept
rec_cols <- c("numTombo", "collectionCode.new", "catalogNumber", "basisOfRecord", "recordedBy.new",
              "recordNumber.new", "year.new", "family", "scientificName", "taxonName", "taxonAuthor",
              "country.new", "stateProvince.new", "municipality.new", "locality.new", "loc.correct1",
              "resolution.gazetteer", "decimalLatitude.new1", "decimalLongitude.new1", "is_nt", "our_efg",
              "tax.check1", "dist.check")
cat("Columns not found:", setdiff(rec_cols, names(final_df_all)), "\n")
checklist_records <- final_df_all[, intersect(rec_cols, names(final_df_all))]
checklist_records$dataset <- ifelse(final_df_all$checklist_source %in% "interpolation", "spatial", "extra")
print(table(checklist_records$dataset))
saveRDS(checklist_records, here::here("data", "derived-data", "rds", "checklist_records.rds"))
rm(rec_cols, checklist_records)

##### Save the final_df_endemism checklist #####
checklists <- list()

final_df_endemism1 <- tidyr::separate_longer_delim(final_df_endemism,
                                                   our_efg,
                                                   delim = "|")
final_df_endemism1 <- final_df_endemism1[!is.na(final_df_endemism1$our_efg), ]

##### How many species are near and pure endemics? #####
pure_and_near_efg <- length(unique(final_df_endemism1$scientificName[
  grepl("near-endemic|pure-endemic",final_df_endemism1$spp_is_efg_endemic)]))
# Only pure and proportion
only_pure_efg <- length(unique(final_df_endemism1$scientificName[
  grepl("pure-endemic",final_df_endemism1$spp_is_efg_endemic)]))
only_pure_efg / pure_and_near_efg # 81% PURE!!! for biome endemism set

# Now let's gsub these names to make my life easier
final_df_endemism1$spp_is_efg_endemic <- gsub(" pure-endemic", "", final_df_endemism1$spp_is_efg_endemic)
final_df_endemism1$spp_is_efg_endemic <- gsub(" near-endemic", "", final_df_endemism1$spp_is_efg_endemic)
final_df_endemism1$spp_is_efg_endemic <- gsub("not endemic", NA, final_df_endemism1$spp_is_efg_endemic)


# Endemics checklist per biome --------------------------------------------
efgs <- unique(final_df_endemism1$spp_is_efg_endemic[!is.na(final_df_endemism1$spp_is_efg_endemic)])

for(i in efgs){
  cat("Checklisting", i, "\n")
  
  biome_subset <- final_df_endemism1[final_df_endemism1$spp_is_efg_endemic %in% i, ]
  biome_subset1 <- biome_subset
  biome_subset1$scientificName <- NULL
  names(biome_subset1)[which(names(biome_subset1) == "taxonName")] <- "scientificName"
  names(biome_subset1)[which(names(biome_subset1) == "taxonAuthor")] <- "scientificNameAuthorship"
  biome_checklist <- plantR::checkList(x = biome_subset1, n.vouch = 10, rm.dup = FALSE)
  biome_checklist$our_efg <- i
  biome_checklist <- biome_checklist[!duplicated(biome_checklist$scientificName), ]
  checklists[[i]] <- biome_checklist
}

# Bind these lines
checklists_final <- dplyr::bind_rows(checklists)

# Order, rename, save
checklists_final <- checklists_final[order(checklists_final$our_efg,
                                           checklists_final$family,
                                           checklists_final$scientificName), ]
names(checklists_final)[which(names(checklists_final) == "family")] <- "family"
names(checklists_final)[which(names(checklists_final) == "scientificName")] <- "species"
names(checklists_final)[which(names(checklists_final) == "scientificNameAuthorship")] <- "author"
names(checklists_final)[which(names(checklists_final) == "our_efg")] <- "biome"
writexl::write_xlsx(checklists_final, 
                    path = file.path("outputs", "tables", "checklist_endemics_biome.xlsx"))

# Aggregate this numbers to left_join later to main table 1
biome_endemics <- aggregate(species ~ biome, data = checklists_final,
                            FUN = function(x) length(unique(x)))

##### How many native species, genus, and families we have in tropical America? #####
# How many registers from each type of source?
nrow(final_df_all) # 1,894,044 registers used


# How many species occur in the tropical America --------------------------

# Checklist first
get_these <- unique(final_df_all$scientificName[final_df_all$is_nt %in% TRUE])
final_df_all_checklist <- final_df_all
final_df_all_checklist$scientificName <- NULL
names(final_df_all_checklist)[which(names(final_df_all_checklist) == "taxonName")] <- "scientificName"
names(final_df_all_checklist)[which(names(final_df_all_checklist) == "taxonAuthor")] <- "scientificNameAuthorship"
neotropical_checklist <- plantR::checkList(x = final_df_all_checklist[final_df_all_checklist$is_nt %in% TRUE, ], n.vouch = 10, rm.dup = FALSE)
neotropical_checklist <- neotropical_checklist[!duplicated(neotropical_checklist$scientificName), ]

length(unique(final_df_all$taxonName[final_df_all$is_nt %in% TRUE])) # we have 26,091 species
all_end <- length(unique(final_df_all$taxonName[final_df_all$is_nt %in% TRUE & # of which 24,637 are endemic
                                            final_df_all$spp_is_nt_endemic %in% c("pure-endemic", "near-endemic")]))
pure_end <- length(unique(final_df_all$taxonName[final_df_all$is_nt %in% TRUE & # 
                                                        final_df_all$spp_is_nt_endemic %in% c("pure-endemic")]))
# How much are pure and near-endemics?
pure_end/all_end # 87% pure, 13% near
# Genera and families
length(unique(gsub(" .*", "", final_df_all$scientificName[final_df_all$is_nt %in% TRUE]))) # 1,960 genera
length(unique(final_df_all$family[final_df_all$is_nt %in% TRUE])) # and 197 families

# Save the checklist at last

# Not endemics: check these
non_end <- unique(final_df_all$scientificName[final_df_all$is_nt %in% TRUE &
                                                final_df_all$spp_is_nt_endemic %in% "not endemic"])
non_end_df <- final_df_all[final_df_all$scientificName %in% non_end, ]
# Only 40 species >= 90 that are supposed to be endemics but not assigned due to
# gazzetteer I guess

##### Species that we could not assign a biome #####
# 3,746 couldn't be assigned a biome
missing_spp <- sort(setdiff(final_df_all$scientificName, 
                            final_df_richness_biome$scientificName))

# Subset them
missing_spp_df <- final_df_all[final_df_all$scientificName %in% missing_spp, ]

# Load data from Flora do Brasil and see if we can assign a ecoregion/biome
file_path <- file.path("G:", "Outros computadores", "Meu laptop (1)",
                       "Doutorado", "SynTreeSys", "Taxonomy", "data", "raw-data")

# FURB
# file_path <- file.path("G:", "Outros computadores", "Meu laptop", 
#                        "SynTreeSys", "Taxonomy", "data", "raw-data")
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

# Keep only species which we were able to assign a biome
# From the 3,746 we werent' able to assign a biome before,
# we recovered a biome for 2,196 doing the steps above (~60%)
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

biome_richness <- data.frame(scientificName = final_df_richness_biome$scientificName,
                             our_efg = final_df_richness_biome$our_efg)
# We were able to assign a biome to 23,607 species
length(unique(biome_richness$scientificName))

biome_richness <- tidyr::separate_longer_delim(biome_richness,
                                               our_efg,
                                               delim = "|")

##### This is for assessments!!! #####
biome_richness_ass <- aggregate(our_efg ~ scientificName,
                                data = biome_richness,
                                FUN = function(x) paste(sort(unique(x)), collapse = "|"))
saveRDS(biome_richness_ass, here::here("data", "derived-data", "rds", "spp_biomes_association.rds"))


biome_richness_agg <- aggregate(scientificName ~ our_efg,
                            data = biome_richness,
                            FUN = function(x) length(unique(x)))
names(biome_richness_agg)[which(names(biome_richness_agg) == "scientificName")] <- "richness"

##### Make the checklist per biome #####
checklists <- list()

final_df_richness_biome1 <- tidyr::separate_longer_delim(final_df_richness_biome,
                                                         our_efg,
                                                         delim = "|")
final_df_richness_biome1 <- final_df_richness_biome1[!is.na(final_df_richness_biome1$our_efg), ]

for(i in unique(final_df_richness_biome1$our_efg)){
  cat("Checklisting", i, "\n")
  
  biome_subset <- final_df_richness_biome1[final_df_richness_biome1$our_efg %in% i, ]
  biome_subset1 <- biome_subset
  biome_subset1$scientificName <- NULL
  names(biome_subset1)[which(names(biome_subset1) == "taxonName")] <- "scientificName"
  names(biome_subset1)[which(names(biome_subset1) == "taxonAuthor")] <- "scientificNameAuthorship"
  biome_checklist <- plantR::checkList(x = biome_subset1, n.vouch = 10, rm.dup = FALSE)
  biome_checklist$our_efg <- i
  
  # Wtf is duplicating #
  # a = biome_checklist[duplicated(biome_checklist$scientificName) | 
  #                       duplicated(biome_checklist$scientificName, fromLast = TRUE), ]
  # For now I am solving like this, but need to fix the names in !Taxonomy!
  # Two accepted names taxon names with different authors in different sources
  # Acrocomia aculeata (Jacq.) Sweet in WCVP
  # Acrocomia aculeata (Jacq.) Lodd. ex Mart. in WFO
  # This creates a minor discrepancy in the number of taxa
  biome_checklist <- biome_checklist[!duplicated(biome_checklist$scientificName), ]
  checklists[[i]] <- biome_checklist
}

# Bind
checklists_final <- dplyr::bind_rows(checklists)
# Edit the names inside the list
new_names <- c("family", "species", "records", "vouchers", "biome")
# Write to Excel with multiple worksheets
writexl::write_xlsx(checklists_final, path = file.path("outputs", "tables", "checklist_richness_biome.xlsx"))

# Join to get endemism levels inside each biome
biome_tab <- dplyr::left_join(biome_richness_agg, biome_endemics,
                              by = c("our_efg" = "biome"))
biome_tab$end_lvl <- 100 * (biome_tab$species / biome_tab$richness)
names(biome_tab) <- c("biome", "spp_rich", "spp_end", "end_level")
biome_tab <- biome_tab[order(biome_tab$spp_rich, decreasing = TRUE), ]
biome_tab$end_level <- round(biome_tab$end_level, 2)
# Biomes with no endemics: 0 instead of NA
biome_tab$spp_end[is.na(biome_tab$spp_end)] <- 0
biome_tab$end_level[is.na(biome_tab$end_level)] <- 0
writexl::write_xlsx(biome_tab, file.path("outputs", "tables", "efg_tab.xlsx"))

##### Biome summary #####

# Single-biome endemics and proportion
# 49% of all neotropica endemics confined to a single-biome
100 * sum(biome_tab$spp_end) / length(unique(final_df_endemism1$scientificName)) 

# Most endemic biomes are lowland/montane/thickets
# How much do they represent on single-biome endemics?
sum(biome_tab$spp_end[grepl("lowland|montane|thickets", biome_tab$biome)]) / 
  sum(biome_tab$spp_end) # 89%


# Checklisting ------------------------------------------------------------

##### Make the checklist for tropical American species #####

final_df_all1 <- final_df_all
# Only occurrences in tropical America
final_df_all1 <- final_df_all1[final_df_all1$is_nt %in% TRUE, ]
final_df_all1$scientificName <- NULL
names(final_df_all1)[which(names(final_df_all1) == "taxonName")] <- "scientificName"
names(final_df_all1)[which(names(final_df_all1) == "taxonAuthor")] <- "scientificNameAuthorship"
# numTombo is not unique (e.g. "US_NA" for specimens without catalog number) and rmDup()
# inside checkList() keeps only one record per numTombo, dropping unrelated species
final_df_all1$numTombo <- make.unique(final_df_all1$numTombo)
nt_checklist <- plantR::checkList(x = final_df_all1, n.vouch = 30, rm.dup = TRUE)
# any(duplicated(nt_checklist$scientificName))

# Keeping for the moment #
nt_checklist <- nt_checklist[!duplicated(nt_checklist$scientificName), ]
nt_checklist <- nt_checklist[order(nt_checklist$family,
                                   nt_checklist$scientificName), ]
# names(nt_checklist)[which(names(nt_checklist) == "family")] <- "family"
names(nt_checklist)[which(names(nt_checklist) == "scientificName")] <- "species"
names(nt_checklist)[which(names(nt_checklist) == "scientificNameAuthorship")] <- "author"
# names(checklists_final)[which(names(checklists_final) == "our_efg")] <- "biome"

# Write to Excel with multiple worksheets
writexl::write_xlsx(nt_checklist, path = file.path("outputs", "tables", "richness_by_nt.xlsx"))

##### Doing the same for endemics #####
final_df_all2 <- final_df_all
final_df_all2 <- final_df_all2[final_df_all2$is_nt %in% TRUE & 
                                 final_df_all2$spp_is_nt_endemic %in% c("near-endemic", "pure-endemic"), ]

final_df_all2$scientificName <- NULL
names(final_df_all2)[which(names(final_df_all2) == "taxonName")] <- "scientificName"
names(final_df_all2)[which(names(final_df_all2) == "taxonAuthor")] <- "scientificNameAuthorship"
nt_checklist_end <- plantR::checkList(x = final_df_all2, n.vouch = 10, rm.dup = FALSE)

# One row per species (author variants of the same binomial)
nt_checklist_end <- nt_checklist_end[!duplicated(nt_checklist_end$scientificName), ]
names(nt_checklist_end)[which(names(nt_checklist_end) == "scientificName")] <- "species"
names(nt_checklist_end)[which(names(nt_checklist_end) == "scientificNameAuthorship")] <- "author"
# Full names (species + author) of the Neotropical endemics, used in 12
nt_species <- unique(trimws(paste(nt_checklist_end$species,
                                  ifelse(is.na(nt_checklist_end$author), "", nt_checklist_end$author))))
saveRDS(nt_species, here::here("data", "derived-data", "nt_species.rds"))
# Write to Excel with multiple worksheets
writexl::write_xlsx(nt_checklist_end, path = file.path("outputs", "tables", "endemics_by_nt.xlsx"))

# Save the dataframes to upload in Zenodo
arrow::write_parquet(final_df_all, sink = here::here("data",
                                                     "derived-data",
                                                     "occ_records.parquet"))


# Figure S1: records kept at each step (here because it needs the counts logged above)
n_steps <- readxl::read_excel(here::here("outputs", "tables", "n_registers_by_family.xlsx"), sheet = "totals")
n_log <- as.data.frame(data.table::fread(here::here("outputs", "n_registers_log.txt")))
n_log <- n_log[n_log$family %in% "ALL", ]
n_log <- n_log[!duplicated(n_log$step, fromLast = TRUE), ]
n_log <- stats::setNames(n_log$n_registers, n_log$step)
# Boxes: main column (spatial dataset), side branch (extra records) and the checklist dataset
flow_nodes <- data.frame(label = c("Downloaded records", "After removing duplicates",
                                   "High taxonomic confidence", "Trees with valid geography",
                                   "Validated locality", "Administrative unit ≤ 12,500 km²",
                                   "With coordinates", "Not cultivated",
                                   "Not outlier outside known range", "Spatial dataset",
                                   "Extra records without coordinates", "Within known distribution",
                                   "Checklist dataset"),
                         n = c(n_steps$n_registers,
                               n_log[c("11_spatial", "11_extra_candidates",
                                       "11_extra_ok_dist", "11_checklist_all")]),
                         x = c(rep(1, 10), 2, 2, 1.5),
                         y = c(10:1, 7, 6, 0))
flow_nodes$text <- paste0(flow_nodes$label, "\n", format(flow_nodes$n, big.mark = ",", trim = TRUE))
# Arrows, with the % of records dropped where the arrow is a filter
flow_edges <- data.frame(from = c(1:9, 3, 11, 12, 10), to = c(2:10, 11, 12, 13, 13))
flow_edges$pct <- round(100 * (1 - flow_nodes$n[flow_edges$to] / flow_nodes$n[flow_edges$from]), 1)
flow_edges$pct[c(10, 12, 13)] <- NA
flow_edges <- cbind(flow_edges,
                    x = flow_nodes$x[flow_edges$from], y = flow_nodes$y[flow_edges$from] - 0.3,
                    xend = flow_nodes$x[flow_edges$to], yend = flow_nodes$y[flow_edges$to] + 0.3)
p_figS1 <- ggplot2::ggplot() +
  ggplot2::geom_segment(data = flow_edges,
                        ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                        arrow = grid::arrow(length = grid::unit(0.15, "cm"), type = "closed")) +
  ggplot2::geom_text(data = flow_edges[!is.na(flow_edges$pct), ],
                     ggplot2::aes(x = (x + xend) / 2 + 0.04, y = (y + yend) / 2,
                                  label = paste0("−", pct, "%")),
                     hjust = 0, size = 3.5, colour = "grey30") +
  ggplot2::geom_label(data = flow_nodes, ggplot2::aes(x = x, y = y, label = text), size = 3.8) +
  ggplot2::scale_x_continuous(limits = c(0.4, 2.6)) +
  ggplot2::theme_void(base_size = 15)
ggplot2::ggsave(here::here("figures", "FigureS1.svg"), p_figS1, width = 10, height = 8, bg = "white")
ggplot2::ggsave(here::here("figures", "FigureS1.png"), p_figS1, width = 10, height = 8, dpi = 300, bg = "white")
# PDF (vector) for PNAS
ggplot2::ggsave(here::here("figures", "FigureS1.pdf"), p_figS1, width = 10, height = 8, bg = "white")
rm(n_steps, n_log, flow_nodes, flow_edges)

# Species without a biome --------------------------------------------------
# Species of the tropical American checklist (with curated records in tropical
# America) that could not be assigned to any biome, neither by their records nor by
# the vegetation type/domain of the FFB and taxonomy sources
unk <- final_df_all[final_df_all$is_nt %in% TRUE &
                      !final_df_all$taxonName %in% final_df_richness_biome$taxonName, ]
unk_tab <- do.call(rbind, lapply(split(unk, unk$taxonName), function(x)
  data.frame(family = x$family[1],
             species = paste(unique(x$scientificName), collapse = " | "),
             author = x$taxonAuthor[1],
             n_records = nrow(x),
             countries = paste(sort(unique(stats::na.omit(x$country.new))), collapse = "|"),
             biome_status = "biome_unknown")))
unk_tab <- unk_tab[order(unk_tab$family, unk_tab$species), ]
writexl::write_xlsx(unk_tab, here::here("outputs", "tables", "biome_unknown.xlsx"))
rm(unk)

# Probable occurrences: species without curated records in tropical America --
# Accepted tree species of the Americas (Taxonomy checklist) with no curated record
# in tropical America (as the 'probable occurrences' of Lima et al. 2020). Their
# native range in the WCVP (botanical countries, WGSRPD level 3) is crossed with our
# tropical America: a botanical country is tropical if >= 90% of its area is inside
# tropical America (same threshold used for endemism and administrative units),
# extra-tropical if <= 10%, and transitional otherwise
no_rec <- our_names[our_names$our_accepted_taxonRank %in% "species" &
                      !our_names$our_accepted_taxonName %in%
                      final_df_all$taxonName[final_df_all$is_nt %in% TRUE], ]
no_rec <- no_rec[!duplicated(no_rec$our_accepted_taxonName), ]
# WCVP accepted ID of each binomial (accepted names first; synonyms point to their accepted name)
wcvp <- rWCVPdata::wcvp_names[rWCVPdata::wcvp_names$taxon_name %in% no_rec$our_accepted_taxonName, ]
wcvp <- wcvp[order(wcvp$taxon_name, !wcvp$taxon_status %in% "Accepted"), ]
wcvp <- wcvp[!duplicated(wcvp$taxon_name), ]
wcvp$acc_id <- ifelse(is.na(wcvp$accepted_plant_name_id), wcvp$plant_name_id, wcvp$accepted_plant_name_id)
no_rec$acc_id <- wcvp$acc_id[match(no_rec$our_accepted_taxonName, wcvp$taxon_name)]
# Native, extant and certain distribution only
dist <- rWCVPdata::wcvp_distributions
dist <- dist[dist$plant_name_id %in% no_rec$acc_id & dist$introduced %in% 0 &
               dist$extinct %in% 0 & dist$location_doubtful %in% 0, ]
# Share of each American botanical country inside tropical America
ta <- terra::vect(here::here("data", "derived-data", "maps", "NTAmericas_efgs.shp"))
ta <- terra::aggregate(terra::project(ta[ta$Morrone_re %in% "Neotropical", ], "epsg:4326"))
l3 <- terra::makeValid(terra::vect(rWCVPdata::wgsrpd3))
l3 <- l3[l3$LEVEL1_COD %in% c(7, 8), ]
l3_in <- terra::intersect(l3, ta)
area_in <- tapply(terra::expanse(l3_in, unit = "km"), l3_in$LEVEL3_COD, sum)
area_l3 <- tapply(terra::expanse(l3, unit = "km"), l3$LEVEL3_COD, sum)
l3_share <- stats::setNames(rep(0, length(area_l3)), names(area_l3))
l3_share[names(area_in)] <- area_in / area_l3[names(area_in)]
# NA for botanical countries outside the Americas
dist$share_ta <- as.numeric(l3_share[dist$area_code_l3])
# Taxonomy sources listing each species (union over all its input names)
src_names <- tapply(our_names$source, our_names$our_accepted_taxonName, function(z)
  paste(sort(unique(unlist(strsplit(z, " | ", fixed = TRUE)))), collapse = " | "))
no_rec_tab <- do.call(rbind, lapply(seq_len(nrow(no_rec)), function(i) {
  d <- dist[dist$plant_name_id %in% no_rec$acc_id[i], ]
  s <- d$share_ta[!is.na(d$share_ta)]
  data.frame(family = no_rec$our_accepted_taxonFamily[i],
             species = no_rec$our_accepted_scientificName[i],
             sources = src_names[[no_rec$our_accepted_taxonName[i]]],
             wcvp_areas = paste(sort(unique(d$area)), collapse = "|"),
             max_share_tropical_america = if (length(s) > 0) round(max(s), 3) else NA,
             status = if (nrow(d) == 0) "No WCVP native distribution"
                      else if (length(s) == 0) "Outside the Americas"
                      else if (max(s) >= 0.9) "Probable occurrence"
                      else if (max(s) > 0.1) "Transitional"
                      else if (all(d$continent_code_l1[!is.na(d$share_ta)] %in% 7)) "Extra-tropical (North America)"
                      else "Extra-tropical (southern South America)",
             distribution_sources = no_rec$geographicalDistribution_aggregated[i],
             domain_sources = no_rec$domain_aggregated[i])
}))
no_rec_tab <- no_rec_tab[order(no_rec_tab$status, no_rec_tab$family, no_rec_tab$species), ]
writexl::write_xlsx(no_rec_tab, here::here("outputs", "tables", "probable_occurrence.xlsx"))
print(table(no_rec_tab$status))
rm(no_rec, wcvp, dist, ta, l3, l3_in, area_in, area_l3, l3_share, src_names)

# Numbers for the manuscript ----------------------------------------------
n_log <- as.data.frame(data.table::fread(here::here("outputs", "n_registers_log.txt")))
n_log <- n_log[n_log$family %in% "ALL" & grepl("^11_", n_log$step), ]
n_log <- n_log[!duplicated(n_log$step, fromLast = TRUE), ]
spp_nt <- unique(final_df_all$taxonName[final_df_all$is_nt %in% TRUE])
# Species of tropical America (binomials, as in n_spp_TA) with at least one biome
n_assigned <- length(intersect(spp_nt, final_df_richness_biome$taxonName))
top3 <- biome_tab[match(c("Tropical-subtropical lowland rainforests", "Tropical-subtropical montane rainforests",
                          "Tropical-subtropical dry forests and thickets"), biome_tab$biome), ]
dir.create(here::here("outputs", "numbers"), showWarnings = FALSE, recursive = TRUE)
utils::write.csv(data.frame(key = c(paste0("n_", n_log$step),
                                    "n_spp_TA", "n_genera_TA", "n_families_TA", "pct_world_trees",
                                    "pct_increase_gts", "n_endemic_TA", "pct_endemic_TA", "pct_pure_endemic",
                                    "pct_near_endemic", "n_single_source", "pct_single_source",
                                    "n_spp_assigned_biome", "pct_spp_assigned_biome", "n_spp_biome_unknown", "n_spp_no_records_TA", "n_spp_probable_occurrence", "n_single_biome_endemics",
                                    "pct_single_biome_endemics", "pct_spp_lowland", "pct_sbe_lowland",
                                    "pct_sbe_top3",
                                    paste0(rep(c("n_spp_", "n_end_"), each = 3), make.names(top3$biome))),
                            value = c(n_log$n_registers,
                                      length(spp_nt),
                                      length(unique(gsub(" .*", "", final_df_all$scientificName[final_df_all$is_nt %in% TRUE]))),
                                      length(unique(final_df_all$family[final_df_all$is_nt %in% TRUE])),
                                      100 * length(spp_nt) / 57598,
                                      100 * (length(spp_nt) / 23616 - 1),
                                      all_end, 100 * all_end / length(spp_nt),
                                      100 * pure_end / all_end, 100 * (1 - pure_end / all_end),
                                      length(single_source_names_acc),
                                      100 * length(single_source_names_acc) / nrow(name_counts_acc),
                                      n_assigned, 100 * n_assigned / length(spp_nt), nrow(unk_tab), nrow(no_rec_tab), sum(no_rec_tab$status %in% "Probable occurrence"),
                                      sum(biome_tab$spp_end),
                                      100 * sum(biome_tab$spp_end) / n_assigned,
                                      100 * top3$spp_rich[1] / n_assigned,
                                      100 * top3$spp_end[1] / sum(biome_tab$spp_end),
                                      100 * sum(top3$spp_end) / sum(biome_tab$spp_end),
                                      top3$spp_rich, top3$spp_end),
                            description = c(paste("Records:", n_log$step),
                                            "Tree species in tropical America", "Genera", "Families",
                                            "% of the 57,598 tree species on Earth (GTS)",
                                            "% increase over the 23,616 species accepted for the region",
                                            "Species endemic to tropical America", "% endemic",
                                            "% of endemics that are pure-endemics", "% near-endemics",
                                            "Accepted names listed by a single source",
                                            "% of accepted names listed by a single source",
                                            "Species assigned to a biome", "% of species assigned to a biome",
                                            "Species with records in tropical America but without a biome (biome_unknown.xlsx)",
                                            "Accepted tree species of the Americas without curated records in tropical America (probable_occurrence.xlsx)",
                                            "Probable occurrences: no curated records, WCVP native range in tropical America (>= 90% of a botanical country)",
                                            "Single-biome endemics (Table 1 total)",
                                            "% single-biome endemics (of species assigned to a biome)",
                                            "% of assigned species in lowland rainforests",
                                            "% of single-biome endemics in lowland rainforests",
                                            "% of single-biome endemics in the three top biomes",
                                            paste(rep(c("Species in", "Endemics of"), each = 3), top3$biome))),
                 here::here("outputs", "numbers", "10_preparing_checklist.csv"), row.names = FALSE)
rm(n_log, spp_nt, n_assigned, top3, unk_tab, no_rec_tab)
