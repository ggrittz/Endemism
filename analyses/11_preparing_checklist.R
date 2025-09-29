

# Checklist of tropical American trees ------------------------------------

# Cleaned occs folder
file_path1 <- here::here("data", "derived-data", "Cleaned_occs")

# Load data by family
family_names <- here::here("data", "derived-data", "family_names.rds")
family_names <- readRDS(file.path(family_names))

# Loading species list
our_names <- readRDS(here::here("data", "derived-data", "checklist_standardized.rds"))
our_names <- our_names[!duplicated(our_names$our_accepted_scientificName), ]

# latamMap already classified (see script 06_crossing_maps.R)
file_path <- here::here("data", "derived-data", "maps")
file_name <- "gaz_intersected_efg.shp"
# file_name1 <- "gaz_intersected_nt.shp"
gaz_class_efg <- terra::vect(file.path(file_path, file_name))
gaz_class_efg <- terra::as.data.frame(gaz_class_efg)

# Prepare which countries/states can be used for type specimens
# e.g., species fall into Brazil, so it is endemic to Neotropics
# # state-level
gaz_class_efg$state_clean <- sub("^([^_]+_[^_]+).*", "\\1", gaz_class_efg$loc.correc)

# Get unique our_efg aggregations by cleaned location
agg <- aggregate(our_efg ~ state_clean, 
                 data = gaz_class_efg, 
                 FUN = function(x) paste(unique(x), collapse = "|"))

names(agg)[2] <- "our_efg_state_level"
gaz_class_efg <- dplyr::left_join(gaz_class_efg, agg, by = "state_clean")

##### Do the same at country-level now #####
gaz_class_efg$country_clean <- sub("_.*", "", gaz_class_efg$country_clean)

agg <- aggregate(our_efg ~ country_clean,
                 data = gaz_class_efg,
                 FUN = function(x) paste(unique(x), collapse = "|"))

names(agg)[2] <- "our_efg_country_level"

# Merge back
gaz_class_efg <- dplyr::left_join(gaz_class_efg, agg, by = "country_clean")
gaz_class_efg$state_clean <- NULL
gaz_class_efg$country_clean <- NULL


fam_list <- list()
cols <- c("tax.check1", "geo.check1", "origin.coord1", "cult.check", 
          "country.new","dist.check", "out.check", "scientificName", "scientificNameAuthorship", 
          "family",
          "decimalLatitude.new1", "decimalLongitude.new1",
          # Below are additions to work with plantR::checkList
          "catalogNumber", "collectionCode.new", "recordedBy.new", "recordNumber.new",
          "year.new", "stateProvince.new", "municipality.new", "loc.correct",
          "dup.ID", "dup.prop", "typeStatus", "numTombo", "month", "day")


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




# 


# Make checklist based on taxonomically verified registers
# table(all_data$tax.check1)
checklist <- plantR::checkList(x = fam_list,
                               n.vouch = 30,
                               type = "short",
                               rm.dup = TRUE)

#




