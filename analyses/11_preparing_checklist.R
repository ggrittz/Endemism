

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
# Only trees
our_names <- our_names[grepl("tree", our_names$lifeForm_aggregated), ]
# Only accepted names below
sum(!duplicated(our_names$our_accepted_scientificName)) # 34,399
aggregate(our_accepted_scientificName ~ our_accepted_bb,
          data = our_names[!duplicated(our_names$our_accepted_scientificName),],
          FUN = length) # 96% wfo

##### HOPE THIS CAN BE FIXED VIA-TAXONOMY FIX #####
##### NEED TO RE-RUN EVERYTHING #####
# Which names came from GBIF only?
# a <- our_names[our_names$our_accepted_bb %in% "gbif",]
# a <- a[!duplicated(a$our_accepted_scientificName), ]
# b <- a[, c("input_full_name", "gbif_output_full_name", "wfo_output_full_name", 
#            "our_taxonID", "our_accepted_bb", "our_accepted_scientificName",
#            "status_check", "to_check", "wfo_match_type", "trust_class", "source",
#            "taxonName", "taxonAuthorship", "wfo_scientificName", "wfo_scientificNameAuthorship")]


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

##### Backbones for taxonomy fix #####
# Get WFO parentalID for these cases...
raw_path <- here::here("data", "raw-data", "WFO")
# file_name <- "WFO_backbone.zip"
# if (!dir.exists(raw_path)) 
#   dir.create(raw_path)
# options(timeout = 10000)
# download.file(url = "https://zenodo.org/records/18007552/files/_DwC_backbone_R.zip?download=1",
#               destfile = file.path(raw_path, file_name), mode = "wb")
# unzip(file.path(raw_path, file_name), exdir = raw_path)
wfo <- data.table::fread(file.path(raw_path, "classification.csv"))
# wfo$scientificName_full <- ifelse(wfo$scientificNameAuthorship %in% "",
#                                   wfo$scientificName,
#                                   paste(wfo$scientificName, wfo$scientificNameAuthorship))
# Match scientificNames e puxar a informação de parentNameUsageID na coluna taxonID
wfo_dt <- data.table::as.data.table(wfo)

# WCVP now
raw_path1 <- here::here("data", "raw-data", "WCVP")
wcvp <- data.table::fread(file.path(raw_path1, "wcvp_taxon.csv"))
wcvp_dt <- data.table::as.data.table(wcvp)
rm(wfo, wcvp); gc()


# Load by family, keep what is usable ("high") and is not already on final_df and bind
for(i in family_names){
  
  cat("Loading and filtering", i, "\n")
  gz_path <- file.path(file_path1, i, paste0(i, ".csv.gz"))
  if(!file.exists(gz_path)){
    next
  }
  
  ######################################
  ######################################
  ##### FIX TAXONOMY HERE TOO LMAO #####
  ######################################
  ######################################
  
  
  family_data_bad <- as.data.frame(data.table::fread(gz_path, select = cols))
  
  # START OF TAXONOMY FIX ---------------------------------------------------
  
  
  
  ##### I AM ADDING THIS FIX HERE, FOR THE MOMENT #####
  ##### THIS FIX IS GOING TO BE ADDED TO TAXONOMY #####
  
  # Infraspecific ranks fix -------------------------------------------------
  
  # Get all occurrences that are infraspecific
  get_these <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                           family_data_bad$taxonName, 
                           perl = TRUE)) 
  
  # Get infraspecific names
  infraspecific_names <- family_data_bad$taxonName[get_these]
  
  # Create a lookup table - ONLY RUN IF THERE ARE INFRASPECIFIC NAMES
  if(length(infraspecific_names) > 0) {
    wfo_lookup <- wfo_dt[scientificName %in% infraspecific_names, 
                         .(scientificName, 
                           scientificNameAuthorship,
                           parentNameUsageID,
                           acceptedNameUsageID,
                           taxonomicStatus,
                           taxonRank)]
    
    # Remove unchecked names
    # wfo_lookup <- wfo_lookup[!wfo_lookup$taxonomicStatus %in% "Unchecked", ]
    
    # Use the acceptedID when parentNameUsageID is empty 
    wfo_lookup[parentNameUsageID == "", parentNameUsageID := acceptedNameUsageID]
    
    # Get parent names for infraspecific taxa
    wfo_parents <- wfo_dt[taxonID %in% wfo_lookup[!is.na(parentNameUsageID) & 
                                                    parentNameUsageID != ""]$parentNameUsageID,
                          .(taxonID, 
                            parent_scientificName = scientificName,
                            parent_scientificNameAuthorship = scientificNameAuthorship)]
    
    wfo_lookup <- dplyr::left_join(wfo_lookup, wfo_parents,
                                   by = c("parentNameUsageID" = "taxonID"))
    
    # Deal with synonyms that caused new accepted names to have subsp./var./f.
    get_these1 <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                              wfo_lookup$parent_scientificName,
                              perl = TRUE))
    
    # Only process problematic cases if they exist
    if(length(get_these1) > 0) {
      # wfo_lookup$parent_scientificName[get_these1]
      
      # Deal with synonyms that caused new accepted names to have subsp./var./f.
      get_these1 <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                                wfo_lookup$parent_scientificName,
                                perl = TRUE))
      
      # Get the parent IDs for these problematic cases
      problem_parent_ids <- wfo_lookup$parentNameUsageID[get_these1]
      
      # Get the parents of these problematic parents (the next level up)
      higher_parents <- wfo_dt[taxonID %in% wfo_dt[taxonID %in% problem_parent_ids]$parentNameUsageID,
                               .(taxonID,
                                 parent_scientificName_fixed = scientificName,
                                 parent_scientificNameAuthorship_fixed = scientificNameAuthorship)]
      
      # Get the mapping of which original parent ID maps to which higher parent
      parent_mapping <- wfo_dt[taxonID %in% problem_parent_ids,
                               .(original_parent_id = taxonID,
                                 higher_parent_id = parentNameUsageID)]
      
      # Join to get the fixed names
      parent_mapping <- dplyr::left_join(parent_mapping, higher_parents,
                                         by = c("higher_parent_id" = "taxonID"))
      
      # Update wfo_lookup
      for(k in seq_along(get_these1)) {
        row_idx <- get_these1[k]
        original_parent_id <- wfo_lookup$parentNameUsageID[row_idx]
        
        match_idx <- which(parent_mapping$original_parent_id == original_parent_id)
        
        if(length(match_idx) > 0) {
          wfo_lookup$parent_scientificName[row_idx] <- parent_mapping$parent_scientificName_fixed[match_idx]
          wfo_lookup$parent_scientificNameAuthorship[row_idx] <- parent_mapping$parent_scientificNameAuthorship_fixed[match_idx]
        }
      }
    }
    
    # When there are duplicates, keep only the accepted
    wfo_lookup <- wfo_lookup[order(taxonomicStatus != "Accepted")]
    wfo_lookup <- wfo_lookup[!duplicated(scientificName), ]
    
    ##### FINALLY SUBSTITUTE THE NAMES #####
    
    # Named vector for quick lookup
    parent_name_lookup <- setNames(wfo_lookup$parent_scientificName, wfo_lookup$scientificName)
    parent_author_lookup <- setNames(wfo_lookup$parent_scientificNameAuthorship, wfo_lookup$scientificName)
    
    # Find matches
    matches <- family_data_bad$taxonName %in% names(parent_name_lookup)
    
    # For rows with matches, update taxonName and taxonAuthor
    if(sum(matches) > 0) {
      # Get the matching scientific names
      matching_names <- family_data_bad$taxonName[matches]
      # Update taxonName with parent_scientificName
      family_data_bad$taxonName[matches] <- parent_name_lookup[matching_names]
      # Update taxonAuthor with parent_scientificNameAuthorship
      family_data_bad$taxonAuthor[matches] <- parent_author_lookup[matching_names]
      # Recreate scientificName column
      family_data_bad$scientificName[matches] <- ifelse(
        !is.na(family_data_bad$taxonAuthor[matches]) & family_data_bad$taxonAuthor[matches] != "",
        paste(family_data_bad$taxonName[matches], family_data_bad$taxonAuthor[matches]),
        family_data_bad$taxonName[matches]
      )
    }
    
    
    
    # Remove unchecked names now (they become NA on taxonName, taxonAuthor etc.)
    # family_data <- family_data[!is.na(family_data$taxonName), ]
  } else {
    cat("  No infraspecific names found for WFO fix in", i, "- skipping\n")
  }
  
  ###########################################################
  ###########################################################
  ##### DOING THE SAME BUT FOR NAMES ONLY FOUND IN WCVP #####
  ###########################################################
  ###########################################################
  
  # Infraspecific ranks fix -------------------------------------------------
  
  # Get all occurrences that are infraspecific
  get_these <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                           family_data_bad$taxonName, 
                           perl = TRUE)) 
  
  # Get infraspecific names
  infraspecific_names <- family_data_bad$taxonName[get_these]
  
  # Create a lookup table - ONLY RUN IF THERE ARE INFRASPECIFIC NAMES
  if(length(infraspecific_names) > 0) {
    wcvp_lookup <- wcvp_dt[scientfiicname %in% infraspecific_names, 
                           .(scientfiicname, 
                             scientfiicnameauthorship,
                             parentnameusageid,
                             acceptednameusageid,
                             taxonomicstatus,
                             taxonrank)]
    
    # Use the acceptedID when parentNameUsageID is empty 
    wcvp_lookup[parentnameusageid == "", parentnameusageid := acceptednameusageid]
    
    # Get parent names for infraspecific taxa
    wcvp_parents <- wcvp_dt[taxonid %in% wcvp_lookup[!is.na(parentnameusageid) & 
                                                       parentnameusageid != ""]$parentnameusageid,
                            .(taxonid, 
                              parent_scientificName = scientfiicname,
                              parent_scientificNameAuthorship = scientfiicnameauthorship)]
    
    wcvp_lookup <- dplyr::left_join(wcvp_lookup, wcvp_parents,
                                    by = c("parentnameusageid" = "taxonid"))
    
    # Deal with synonyms that caused new accepted names to have subsp./var./f.
    get_these1 <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                              wcvp_lookup$parent_scientificName,
                              perl = TRUE))
    
    # Only process problematic cases if they exist
    if(length(get_these1) > 0) {
      # wfo_lookup$parent_scientificName[get_these1]
      
      # Deal with synonyms that caused new accepted names to have subsp./var./f.
      get_these1 <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                                wcvp_lookup$parent_scientificName,
                                perl = TRUE))
      
      # Get the parent IDs for these problematic cases
      problem_parent_ids <- wcvp_lookup$parentnameusageid[get_these1]
      
      # Get the parents of these problematic parents (the next level up)
      higher_parents <- wcvp_dt[taxonid %in% wcvp_dt[taxonid %in% problem_parent_ids]$parentnameusageid,
                                .(taxonid,
                                  parent_scientificName_fixed = scientfiicname,
                                  parent_scientificNameAuthorship_fixed = scientfiicnameauthorship)]
      
      # Get the mapping of which original parent ID maps to which higher parent
      parent_mapping <- wcvp_dt[taxonid %in% problem_parent_ids,
                                .(original_parent_id = taxonid,
                                  higher_parent_id = parentnameusageid)]
      
      # Join to get the fixed names
      parent_mapping <- dplyr::left_join(parent_mapping, higher_parents,
                                         by = c("higher_parent_id" = "taxonid"))
      
      # Update wcvp_lookup
      for(k in seq_along(get_these1)) {
        row_idx <- get_these1[k]
        original_parent_id <- wcvp_lookup$parentnameusageid[row_idx]
        
        match_idx <- which(parent_mapping$original_parent_id == original_parent_id)
        
        if(length(match_idx) > 0) {
          wcvp_lookup$parent_scientificName[row_idx] <- parent_mapping$parent_scientificName_fixed[match_idx]
          wcvp_lookup$parent_scientificNameAuthorship[row_idx] <- parent_mapping$parent_scientificNameAuthorship_fixed[match_idx]
        }
      }
    }
    
    # When there are duplicates, keep only the accepted
    wcvp_lookup <- wcvp_lookup[order(taxonomicstatus != "Accepted")]
    wcvp_lookup <- wcvp_lookup[!duplicated(scientfiicname), ]
    
    ##### FINALLY SUBSTITUTE THE NAMES #####
    
    # Named vector for quick lookup
    parent_name_lookup <- setNames(wcvp_lookup$parent_scientificName, wcvp_lookup$scientfiicname)
    parent_author_lookup <- setNames(wcvp_lookup$parent_scientificNameAuthorship, wcvp_lookup$scientfiicname)
    
    # Find matches
    matches <- family_data_bad$taxonName %in% names(parent_name_lookup)
    
    # For rows with matches, update taxonName and taxonAuthor
    if(sum(matches) > 0) {
      # Get the matching scientific names
      matching_names <- family_data_bad$taxonName[matches]
      # Update taxonName with parent_scientificName
      family_data_bad$taxonName[matches] <- parent_name_lookup[matching_names]
      # Update taxonAuthor with parent_scientificNameAuthorship
      family_data_bad$taxonAuthor[matches] <- parent_author_lookup[matching_names]
      # Recreate scientificName column
      family_data_bad$scientificName[matches] <- ifelse(
        !is.na(family_data_bad$taxonAuthor[matches]) & family_data_bad$taxonAuthor[matches] != "",
        paste(family_data_bad$taxonName[matches], family_data_bad$taxonAuthor[matches]),
        family_data_bad$taxonName[matches]
      )
    }
    
    
  } else {
    cat("  No infraspecific names found for WCVP fix in", i, "- skipping\n")
  }
  
  # Taxonomy fix done -------------------------------------------------------
  
  
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
bad_list <- plantR::checkDist(x = bad_list,
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
##### Putting this back! 04/03/2026 #####
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


# Infraspecific ranks fix -------------------------------------------------

# Get all occurrences that are infraspecific
get_these <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", final_df_all$taxonName, 
                   perl = TRUE)) 

# Get WFO parentalID for these cases...
raw_path <- here::here("data", "raw-data", "WFO")
# file_name <- "WFO_backbone.zip"
# if (!dir.exists(raw_path)) 
#   dir.create(raw_path)
# options(timeout = 10000)
# download.file(url = "https://zenodo.org/records/18007552/files/_DwC_backbone_R.zip?download=1",
#               destfile = file.path(raw_path, file_name), mode = "wb")
# unzip(file.path(raw_path, file_name), exdir = raw_path)
wfo <- data.table::fread(file.path(raw_path, "classification.csv"))
# wfo$scientificName_full <- ifelse(wfo$scientificNameAuthorship %in% "",
#                                   wfo$scientificName,
#                                   paste(wfo$scientificName, wfo$scientificNameAuthorship))
# Match scientificNames e puxar a informação de parentNameUsageID na coluna taxonID
wfo_dt <- data.table::as.data.table(wfo)

# Get infraspecific names
infraspecific_names <- final_df_all$taxonName[get_these]

# Create a lookup table
wfo_lookup <- wfo_dt[scientificName %in% infraspecific_names, 
                     .(scientificName, 
                       scientificNameAuthorship,
                       parentNameUsageID,
                       acceptedNameUsageID,
                       taxonomicStatus,
                       taxonRank)]

# Remove unchecked names
wfo_lookup <- wfo_lookup[!wfo_lookup$taxonomicStatus %in% "Unchecked", ]

# Use the acceptedID when parentNameUsageID is empty 
wfo_lookup[parentNameUsageID == "", parentNameUsageID := acceptedNameUsageID]

# Get parent names for infraspecific taxa
wfo_parents <- wfo_dt[taxonID %in% wfo_lookup[!is.na(parentNameUsageID) & 
                                                parentNameUsageID != ""]$parentNameUsageID,
                      .(taxonID, 
                        parent_scientificName = scientificName,
                        parent_scientificNameAuthorship = scientificNameAuthorship)]

wfo_lookup <- dplyr::left_join(wfo_lookup, wfo_parents,
                               by = c("parentNameUsageID" = "taxonID"))

# Deal with synonyms that caused new accepted names to have subsp./var./f.
get_these1 <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                          wfo_lookup$parent_scientificName,
                          perl = TRUE))
wfo_lookup$parent_scientificName[get_these1]

# Deal with synonyms that caused new accepted names to have subsp./var./f.
get_these1 <- which(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
                          wfo_lookup$parent_scientificName,
                          perl = TRUE))

# Get the parent IDs for these problematic cases
problem_parent_ids <- wfo_lookup$parentNameUsageID[get_these1]

# Get the parents of these problematic parents (the next level up)
higher_parents <- wfo_dt[taxonID %in% wfo_dt[taxonID %in% problem_parent_ids]$parentNameUsageID,
                         .(taxonID,
                           parent_scientificName_fixed = scientificName,
                           parent_scientificNameAuthorship_fixed = scientificNameAuthorship)]

# Get the mapping of which original parent ID maps to which higher parent
parent_mapping <- wfo_dt[taxonID %in% problem_parent_ids,
                         .(original_parent_id = taxonID,
                           higher_parent_id = parentNameUsageID)]

# Join to get the fixed names
parent_mapping <- dplyr::left_join(parent_mapping, higher_parents,
                        by = c("higher_parent_id" = "taxonID"))

# Update wfo_lookup
for(i in seq_along(get_these1)) {
  row_idx <- get_these1[i]
  original_parent_id <- wfo_lookup$parentNameUsageID[row_idx]
  
  match_idx <- which(parent_mapping$original_parent_id == original_parent_id)
  
  if(length(match_idx) > 0) {
    wfo_lookup$parent_scientificName[row_idx] <- parent_mapping$parent_scientificName_fixed[match_idx]
    wfo_lookup$parent_scientificNameAuthorship[row_idx] <- parent_mapping$parent_scientificNameAuthorship_fixed[match_idx]
  }
}

# Quick check
any(grepl(" var\\. | subsp\\. | f\\. | fo\\. ", 
          wfo_lookup$parent_scientificName,
          perl = TRUE))

##### FINALLY SUBSTITUTE THE NAMES #####

# First, create a named vector for quick lookup
parent_name_lookup <- setNames(wfo_lookup$parent_scientificName, wfo_lookup$scientificName)
parent_author_lookup <- setNames(wfo_lookup$parent_scientificNameAuthorship, wfo_lookup$scientificName)

# Find matches between final_df_all$taxonName and wfo_lookup$scientificName
matches <- final_df_all$taxonName %in% names(parent_name_lookup)

# For rows with matches, update taxonName and taxonAuthor
if(sum(matches) > 0) {
  # Get the matching scientific names
  matching_names <- final_df_all$taxonName[matches]
  
  # Update taxonName with parent_scientificName
  final_df_all$taxonName[matches] <- parent_name_lookup[matching_names]
  
  # Update taxonAuthor with parent_scientificNameAuthorship
  final_df_all$taxonAuthor[matches] <- parent_author_lookup[matching_names]
}

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
final_df_richness_biome <- final_df_richness_biome[final_df_richness_biome$dist.check %in%
                                                     "ok_dist", ]

# Columns to be used later
# keep_cols <- names(bad_list)

# Endemism per efg, recalculated
final_df_endemism <- final_df_all[!grepl("Transition|predominant", 
                                              final_df_all$our_efg), ]
# Also remove bad resolution cases
final_df_endemism <- final_df_endemism[final_df_endemism$bad_reso %in% c(NA, FALSE), ]

# Remove bad_dist from here
final_df_endemism <- final_df_endemism[final_df_endemism$dist.check %in% "ok_dist", ]

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
orig_to_suggestedFamily <- setNames(synonym_lookup$suggestedName,
                                       synonym_lookup$orig.name)

# Get them
get_these <- which(final_df_all$scientificName %in% names(orig_to_suggestedName))

# Finally, substitute in all dfs 
final_df_all$taxonName[get_these] <- orig_to_suggestedName[final_df_all$scientificName[get_these]]
final_df_all$taxonAuthor[get_these] <- orig_to_suggestedAuthorship[final_df_all$scientificName[get_these]]
final_df_all$scientificName[get_these] <- orig_to_scientificNameFull[final_df_all$scientificName[get_these]]
final_df_all$family[get_these] <- orig_to_suggestedFamily[final_df_all$scientificName[get_these]]

# From richness dataset
get_these <- which(final_df_richness_biome$scientificName %in% names(orig_to_suggestedName))
final_df_richness_biome$taxonName[get_these] <- orig_to_suggestedName[final_df_richness_biome$scientificName[get_these]]
final_df_richness_biome$taxonAuthor[get_these] <- orig_to_suggestedAuthorship[final_df_richness_biome$scientificName[get_these]]
final_df_richness_biome$scientificName[get_these] <- orig_to_scientificNameFull[final_df_richness_biome$scientificName[get_these]]
final_df_richness_biome$family[get_these] <- orig_to_suggestedFamily[final_df_richness_biome$scientificName[get_these]]
# From endemism dataset
get_these <- which(final_df_endemism$scientificName %in% names(orig_to_suggestedName))
final_df_endemism$taxonName[get_these] <- orig_to_suggestedName[final_df_endemism$scientificName[get_these]]
final_df_endemism$taxonAuthor[get_these] <- orig_to_suggestedAuthorship[final_df_endemism$scientificName[get_these]]
final_df_endemism$scientificName[get_these] <- orig_to_scientificNameFull[final_df_endemism$scientificName[get_these]]
final_df_endemism$family[get_these] <- orig_to_suggestedFamily[final_df_endemism$scientificName[get_these]]

# Keeping only tax.check1 == high in all dfs...
final_df_all <- final_df_all[final_df_all$tax.check1 %in% "high", ]
final_df_richness_biome <- final_df_richness_biome[final_df_richness_biome$tax.check1 %in% "high", ]
final_df_endemism <- final_df_endemism[final_df_endemism$tax.check1 %in% "high", ]

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

# Keeping for the moment #
# nt_checklist_end <- nt_checklist_end[!duplicated(nt_checklist_end$scientificName), ]
names(nt_checklist_end)[which(names(nt_checklist_end) == "scientificName")] <- "species"
names(nt_checklist_end)[which(names(nt_checklist_end) == "scientificNameAuthorship")] <- "author"
saveRDS(nt_checklist_end$Species, here::here("data", "derived-data", "nt_species.rds"))
# Write to Excel with multiple worksheets
writexl::write_xlsx(nt_checklist_end, path = file.path("outputs", "tables", "endemics_by_nt.xlsx"))

# Save the dataframes to upload in Zenodo
arrow::write_parquet(final_df_all, sink = here::here("data",
                                                     "derived-data",
                                                     "occ_records.parquet"))
